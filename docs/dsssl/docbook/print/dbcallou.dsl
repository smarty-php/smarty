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
;; - You can't point to an AREASET, that doesn't make any sense
;;   since it would imply a one-to-many link
;;
;; There's still no support for a stylesheet drawing the callouts on a
;; GRAPHIC, and I don't think there ever will be.
;;

(element areaspec (empty-sosofo))
(element area (empty-sosofo))
(element areaset (empty-sosofo))

(element co
  ($callout-mark$ (current-node)))

(element programlistingco ($informal-object$))
(element screenco ($informal-object$))
(element graphicco ($informal-object$))

(element (screenco screen) 
  ($callout-verbatim-display$ %indent-screen-lines% %number-screen-lines%))

(element (programlistingco programlisting) 
  ($callout-verbatim-display$ %indent-programlisting-lines%
			      %number-programlisting-lines%))

;; ----------------------------------------------------------------------

(define ($callout-bug$ conumber)
  (if (and conumber %callout-fancy-bug%)
      (case conumber
	((1) (literal "\dingbat-negative-circled-sans-serif-digit-one;"))
	((2) (literal "\dingbat-negative-circled-sans-serif-digit-two;"))
	((3) (literal "\dingbat-negative-circled-sans-serif-digit-three;"))
	((4) (literal "\dingbat-negative-circled-sans-serif-digit-four;"))
	((5) (literal "\dingbat-negative-circled-sans-serif-digit-five;"))
	((6) (literal "\dingbat-negative-circled-sans-serif-digit-six;"))
	((7) (literal "\dingbat-negative-circled-sans-serif-digit-seven;"))
	((8) (literal "\dingbat-negative-circled-sans-serif-digit-eight;"))
	((9) (literal "\dingbat-negative-circled-sans-serif-digit-nine;"))
	(else (make sequence
		font-weight: 'bold
		(literal "(" (format-number conumber "1") ")"))))
      (make sequence
	font-weight: 'bold
	(if conumber
	    (literal "(" (format-number conumber "1") ")")
	    (literal "(??)")))))

(define ($callout-mark$ co)
  ;; Print the callout mark for co
  (if (equal? (gi co) (normalize "co"))
      ($callout-bug$ (if (node-list-empty? co)
			 #f
			 (child-number co)))
      (let ((areanum (if (node-list-empty? co)
			 #f
			 (if (equal? (gi (parent co)) (normalize "areaset"))
			     (absolute-child-number (parent co))
			     (absolute-child-number co)))))
	($callout-bug$ (if (node-list-empty? co)
			   #f
			   areanum)))))

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
		  (literal "\no-break-space;")
		  (loop (+ atcol 1)))))
	  ($callout-mark$ area))
	($callout-mark$ area))))

(define ($callout-linespecific-content$ indent line-numbers?)
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

(define ($callout-verbatim-display$ indent line-numbers?)
  (let* ((width-in-chars (if (attribute-string "width")
			     (string->number (attribute-string "width"))
			     80)) ;; seems like a reasonable default...
	 (fsize (lambda () (if %verbatim-size-factor%
			       (* (inherited-font-size) %verbatim-size-factor%)
			       (/ (/ (- %text-width% (inherited-start-indent))
				     width-in-chars) 0.7)))))
    (make paragraph
	  space-before: (if (INLIST?) %para-sep% %block-sep%)
	  space-after:  (if (INLIST?) %para-sep% %block-sep%)
	  font-family-name: %mono-font-family%
	  font-size: (fsize)
	  font-weight: 'medium
	  font-posture: 'upright
	  line-spacing: (* (fsize) %line-spacing-factor%)
	  start-indent: (inherited-start-indent)
	  lines: 'asis
          input-whitespace-treatment: 'preserve
	  quadding: 'start
	  ($callout-linespecific-content$ indent line-numbers?))))

;; EOF dbcallout.dsl