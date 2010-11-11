;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; The support provided below is a little primitive because there's no way
;; to do line-addressing in Jade.
;;
;; CO's are supported with the CO element or, in SCREENCO and 
;; PROGRAMLISTINGCO only, AREAs.
;;
;; Notes on the use of AREAs:
;;
;; - Processing is very slow. Jade loops through each AREA for
;;   every column on every line.
;; - Only the LINECOLUMN units are supported, and they are #IMPLIED
;; - If a COORDS only specifies a line, the %callout-default-col% will
;;   be used for the column.
;; - If the column is beyond the end of the line, that will work OK, but
;;   if more than one callout has to get placed beyond the end of the same
;;   line, that doesn't work so well.
;; - Embedded tabs foul up the column counting.
;; - Embedded markup fouls up the column counting.
;; - Embedded markup with embedded line breaks fouls up the line counting.
;; - The callout bugs occur immediately before the LINE COLUMN specified.
;; - You can't point to an AREASET, that doesn't make any sense in HTML
;;   since it would imply a one-to-many link
;;
;; There's still no support for a stylesheet drawing the callouts on a
;; GRAPHIC, and I don't think there ever will be.
;;

(element areaspec (empty-sosofo))
(element area (empty-sosofo))
(element areaset (empty-sosofo))

(element co
  ($callout-mark$ (current-node) #t))

(element programlistingco (process-children))
(element screenco (process-children))
(element graphicco (process-children))

(element (screenco screen) 
  ($callout-verbatim-display$ %indent-screen-lines% %number-screen-lines%))

(element (programlistingco programlisting) 
  ($callout-verbatim-display$ %indent-programlisting-lines% 
			      %number-programlisting-lines%))

;; ----------------------------------------------------------------------

(define ($callout-bug$ conumber)
  (let ((number (if conumber (format-number conumber "1") "0")))
    (if conumber
	(if %callout-graphics%
	    (if (<= conumber %callout-graphics-number-limit%)
		(make empty-element gi: "IMG"
		      attributes: (list (list "SRC" 
					      (root-rel-path
					       (string-append
						%callout-graphics-path%
						number
						%callout-graphics-extension%)))
					(list "HSPACE" "0")
					(list "VSPACE" "0")
					(list "BORDER" "0")
					(list "ALT"
					      (string-append
					       "(" number ")"))))
		(make element gi: "B"
		      (literal "(" (format-number conumber "1") ")")))
	    (make element gi: "B"
		  (literal "(" (format-number conumber "1") ")")))
	(make element gi: "B"
	      (literal "(??)")))))

(define ($callout-mark$ co anchor?)
  ;; Print the callout mark for co
  (let* ((id (attribute-string (normalize "id") co))
	 (attr (if anchor?
		   (list (list "NAME" id))
		   (list (list "HREF" (href-to co))))))
    (make element gi: "A"
	  attributes: attr
	  (if (equal? (gi co) (normalize "co"))
	      ($callout-bug$ (if (node-list-empty? co)
				 #f
				 (child-number co)))
	      (let ((areanum (if (node-list-empty? co)
				 0
				 (if (equal? (gi (parent co)) (normalize "areaset"))
				     (absolute-child-number (parent co))
				     (absolute-child-number co)))))
		($callout-bug$ (if (node-list-empty? co)
				   #f
				   areanum)))))))

(define ($look-for-callout$ line col #!optional (eol? #f))
  ;; Look to see if a callout should be printed at line col, and print
  ;; it if it should
  (let* ((areaspec (select-elements (children (parent (current-node)))
				    (normalize "areaspec")))
	 (areas    (expand-children (children areaspec) 
				    (list (normalize "areaset")))))
    (let loop ((areanl areas))
      (if (node-list-empty? areanl)
	  (empty-sosofo)
	  (make sequence
	    (if ($callout-area-match$ (node-list-first areanl) line col eol?)
		($callout-area-format$ (node-list-first areanl) line col eol?)
		(empty-sosofo))
	    (loop (node-list-rest areanl)))))))

(define ($callout-area-match$ area line col eol?)
  ;; Does AREA area match line col?
  (let* ((coordlist (split (attribute-string (normalize "coords") area)))
	 (aline (string->number (car coordlist)))
	 (acol  (if (null? (cdr coordlist))
		    #f
		    (string->number (car (cdr coordlist)))))
	 (units (if (inherited-attribute-string (normalize "units") area)
		    (inherited-attribute-string (normalize "units") area)
		    (normalize "linecolumn"))))
    (and (equal? units (normalize "linecolumn"))
	 (or
	  (and (equal? line aline)
	       (equal? col acol))
	  (and (equal? line aline)
	       eol? 
	       (or (not acol) (> acol col)))))))

(define ($callout-area-format$ area line col eol?)
  ;; Format AREA area at the appropriate place
  (let* ((coordlist (split (attribute-string (normalize "coords") area)))
	 (aline (string->number (car coordlist)))
	 (acol  (if (null? (cdr coordlist))
		    #f
		    (string->number (car (cdr coordlist))))))
    (if (and (equal? line aline)
	     eol? 
	     (or (not acol) (> acol col)))
	(make sequence
	  (let loop ((atcol col))
	    (if (>= atcol (if acol acol %callout-default-col%))
		(empty-sosofo)
		(make sequence
		  (literal " ")
		  (loop (+ atcol 1)))))
	  ($callout-mark$ area #t))
	($callout-mark$ area #t))))

(define ($callout-verbatim-display$ indent line-numbers?)
  (let* ((content (make element gi: "PRE"
			attributes: (list
				     (list "CLASS" (gi)))
			($callout-verbatim-content$ indent line-numbers?))))
    (if %shade-verbatim%
	(make element gi: "TABLE"
	      attributes: ($shade-verbatim-attr$)
	      (make element gi: "TR"
		    (make element gi: "TD"
			  content)))
	content)))

(define ($callout-verbatim-content$ indent line-numbers?)
  ;; Print linespecific content in a callout with line numbers
  (make sequence
    ($line-start$ indent line-numbers? 1)
    (let loop ((kl (children (current-node)))
	       (linecount 1)
	       (colcount 1)
	       (res (empty-sosofo)))
      (if (node-list-empty? kl)
	  (sosofo-append res
			 ($look-for-callout$ linecount colcount #t)
			 (empty-sosofo))
	  (loop
	   (node-list-rest kl)
	   (if (char=? (node-property 'char (node-list-first kl)
				      default: #\U-0000) #\U-000D)
	       (+ linecount 1)
	       linecount)
	   (if (char=? (node-property 'char (node-list-first kl)
				      default: #\U-0000) #\U-000D)
	       1
	       (if (char=? (node-property 'char (node-list-first kl)
					  default: #\U-0000) #\U-0000)
		   colcount
		   (+ colcount 1)))
	   (let ((c (node-list-first kl)))
	     (if (char=? (node-property 'char c default: #\U-0000)
			 #\U-000D)
		 (sosofo-append res
				($look-for-callout$ linecount colcount #t)
				(process-node-list c)
				($line-start$ indent
					      line-numbers?
					      (+ linecount 1)))
		 (sosofo-append res
				($look-for-callout$ linecount colcount)
				(process-node-list c)))))))))

;; EOF dbcallout.dsl

