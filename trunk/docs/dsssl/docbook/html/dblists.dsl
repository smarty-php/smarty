;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; =============================== LISTS ================================

(element orderedlist
  (let* ((depth (length (hierarchical-number-recursive 
			 (normalize "orderedlist"))))
	 (numeration (attribute-string (normalize "numeration")))
	 (firstitem (node-list-first 
		     (select-elements (children (current-node))
				      (normalize "listitem"))))
	 (start  (orderedlist-listitem-number firstitem))
	 (rawnum (cond
		  ((equal? numeration (normalize "arabic")) 0)
		  ((equal? numeration (normalize "loweralpha")) 1)
		  ((equal? numeration (normalize "lowerroman")) 2)
		  ((equal? numeration (normalize "upperalpha")) 3)
		  ((equal? numeration (normalize "upperroman")) 4)
		  (else (modulo depth 5))))
	 (type (case rawnum
		 ((0) "1")
		 ((1) "a")
		 ((2) "i")
		 ((3) "A")
		 ((4) "I"))))
    (make sequence
      (if %spacing-paras%
	  (make element gi: "P" (empty-sosofo))
	  (empty-sosofo))
      (para-check)
      (process-node-list (select-elements (children (current-node))
					  (normalize "title")))
      (make element gi: "OL"
	    attributes: (append
			 (if (equal? start 1)
			     '()
			     (list (list "START" (number->string start))))
			 (if (equal? (attribute-string (normalize "spacing"))
				     (normalize "compact"))
			     '(("COMPACT" "COMPACT"))
			     '())
			 (list (list "TYPE" type)))
	    (process-node-list (select-elements (children (current-node))
						(normalize "listitem"))))
      (para-check 'restart))))

(element (orderedlist title)
  (make element gi: "P"
	(make element gi: "B"
	      (process-children))))

(element itemizedlist
  (make sequence
    (if %spacing-paras%
	(make element gi: "P" (empty-sosofo))
	(empty-sosofo))
    (para-check)
    (process-node-list (select-elements (children (current-node))
					(normalize "title")))
    (make element gi: "UL"
	  attributes: (if (equal? (attribute-string (normalize "spacing")) (normalize "compact"))
			  '(("COMPACT" "COMPACT"))
			  '())
	  (process-node-list (select-elements (children (current-node))
					      (normalize "listitem"))))
    (para-check 'restart)))

(element listitem
  (let* ((override (inherited-attribute-string (normalize "override")))
	 (mark     (inherited-attribute-string (normalize "mark")))
	 (usemark  (if override override mark))
	 (cssmark  (if (and usemark (assoc usemark %css-liststyle-alist%))
		       (car (cdr (assoc usemark %css-liststyle-alist%)))
		       usemark))
	 (cssstyle (if (and %css-decoration% cssmark)
		       (list (list "STYLE" 
				   (string-append "list-style-type: "
						  cssmark)))
		       '())))
  (make element gi: "LI"
	attributes: cssstyle
	(if (attribute-string (normalize "id"))
	    (make element gi: "A"
		  attributes: (list
			       (list "NAME" (attribute-string (normalize "id"))))
		  (empty-sosofo))
	    (empty-sosofo))
	(process-children))))

(element (orderedlist listitem simpara)
  (let* ((spacing    (inherited-attribute-string (normalize "spacing")))
	 (listitem   (parent (current-node)))
	 (lichildren (node-list-filter-out-pis
		      (children listitem)))
	 (childcount (node-list-length lichildren)))
    (if (and (equal? spacing (normalize "compact"))
	     (equal? childcount 1))
	($paragraph$ "SPAN")
	(next-match))))

(element (itemizedlist listitem simpara)
  (let* ((spacing    (inherited-attribute-string (normalize "spacing")))
	 (listitem   (parent (current-node)))
	 (lichildren (node-list-filter-out-pis
		      (children listitem)))
	 (childcount (node-list-length lichildren)))
    (if (and (equal? spacing (normalize "compact"))
	     (equal? childcount 1))
	($paragraph$ "SPAN")
	(next-match))))

(element variablelist
  (let* ((termlength (if (attribute-string (normalize "termlength"))
			 (string->number 
			  (attribute-string (normalize "termlength")))
			 %default-variablelist-termlength%))
	 (too-long?  (variablelist-term-too-long? termlength)))
    (make sequence
      (if %spacing-paras%
	  (make element gi: "P" (empty-sosofo))
	  (empty-sosofo))
      (para-check)

      (if (and (or (and termlength (not too-long?))
		   %always-format-variablelist-as-table%)
	       (or %may-format-variablelist-as-table%
		   %always-format-variablelist-as-table%))
	  (make element gi: "TABLE"
		attributes: '(("CLASS" "VARIABLELIST")
			      ("BORDER" "0")
			      ("CELLSPACING" "1")
			      ("CELLPADDING" "1"))
		(if %html40%
		    (make element gi: "TBODY"
			  (with-mode variablelist-table
			    (process-children)))
		    (with-mode variablelist-table
		      (process-children))))
	  (make sequence
	    (make element gi: "DIV"
		  attributes: (list (list "CLASS" (gi)))
		  (process-node-list
		   (select-elements (children (current-node)) 
				    (normalize "title")))
		  (make element gi: "DL"
			(process-node-list
			 (select-elements (children (current-node)) 
					  (normalize "varlistentry")))))))
      (para-check 'restart))))

(element varlistentry
  (let ((terms    (select-elements (children (current-node)) (normalize "term")))
	(listitem (select-elements (children (current-node)) (normalize "listitem"))))
    (make sequence
      (make element gi: "DT"
	    (if (attribute-string (normalize "id"))
		(make sequence
		  (make element gi: "A"
			attributes: (list
				     (list "NAME" (attribute-string (normalize "id"))))
			(empty-sosofo))
		  (process-node-list terms))
		(process-node-list terms)))
      (process-node-list listitem))))
  
(element (varlistentry term)
  (make sequence
    (process-children-trim)
    (if (not (last-sibling?))
	(literal ", ")
	(literal ""))))

(element (varlistentry listitem)
  (make element gi: "DD"
	(process-children)))

(mode variablelist-table
  (element (variablelist title)
    (make element gi: "TR"
	  attributes: '(("CLASS" "TITLE"))
	  (make element gi: "TH"
		attributes: '(("ALIGN" "LEFT")
			      ("VALIGN" "TOP")
			      ("COLSPAN" "3"))
		(process-children))))

  (element varlistentry
    (let* ((terms      (select-elements (children (current-node))
					(normalize "term")))
	   (listitem   (select-elements (children (current-node))
					(normalize "listitem")))
	   (termlen    (if (attribute-string (normalize "termlength")
					     (parent (current-node)))
			   (string->number (attribute-string
					    (normalize "termlength")
					    (parent (current-node))))
			   %default-variablelist-termlength%))
	   (too-long? (varlistentry-term-too-long? (current-node) termlen)))
      (if too-long?
	  (make sequence
	    (make element gi: "TR"
		  (make element gi: "TD"
			attributes: '(("ALIGN" "LEFT")
				      ("VALIGN" "TOP")
				      ("COLSPAN" "3"))
			(make element gi: "A"
			      attributes: (list
					   (list "NAME" (element-id)))
			      (empty-sosofo))
			(process-node-list terms)))
	    (make element gi: "TR"
		  (make element gi: "TD"
			attributes: '(("ALIGN" "LEFT")
				      ("VALIGN" "TOP")
				      ("WIDTH" "5"))
			;; where terms would have gone
			(make entity-ref name: "nbsp"))
		  (make element gi: "TD"
			attributes: '(("ALIGN" "LEFT")
				      ("VALIGN" "TOP")
				      ("WIDTH" "5"))
			;; just a little spacer
			(make entity-ref name: "nbsp"))
		  (make element gi: "TD"
			attributes: '(("ALIGN" "LEFT")
				      ("VALIGN" "TOP"))
			(process-node-list listitem))))
	  (make element gi: "TR"
		(make element gi: "TD"
		      attributes: '(("ALIGN" "LEFT")
				    ("VALIGN" "TOP"))
		      (make element gi: "A"
			    attributes: (list
					 (list "NAME" (element-id)))
			    (empty-sosofo))
		      (process-node-list terms))
		(make element gi: "TD"
		      attributes: '(("ALIGN" "LEFT")
				    ("VALIGN" "TOP")
				    ("WIDTH" "5"))
		      ;; just a little spacer
		      (make entity-ref name: "nbsp"))
		(make element gi: "TD"
		      attributes: '(("ALIGN" "LEFT")
				    ("VALIGN" "TOP"))
		      (process-node-list listitem))))))
  
  (element (varlistentry term)
    (make sequence
      (if %css-decoration%
	  (make element gi: "SPAN"
		attributes: '(("STYLE" "white-space: nowrap"))
		(process-children-trim))
	  (make element gi: "NOBR"
		(process-children-trim)))
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element (varlistentry listitem)
    (process-children))
)

(define (simplelist-table majororder cols members)
  (let* ((termcount (node-list-length members))
	 (rows      (quotient (+ termcount (- cols 1)) cols))
	 (htmlrows  (let rowloop ((rownum 1))
		      (if (> rownum rows)
			  (empty-sosofo)
			  (make sequence
			    (simplelist-row rownum majororder
					    rows cols members)
			    (rowloop (+ rownum 1)))))))
    (make sequence
      (if %spacing-paras%
	  (make element gi: "P" (empty-sosofo))
	  (empty-sosofo))
      (make element gi: "TABLE"
	    attributes: '(("BORDER" "0"))
	    (if %html40%
		(make element gi: "TBODY"
		      htmlrows)
		htmlrows))
      (if %spacing-paras%
	  (make element gi: "P" (empty-sosofo))
	  (empty-sosofo)))))

(define (simplelist-row rownum majororder rows cols members)
  (make element gi: "TR"
	(let colloop ((colnum 1))
	  (if (> colnum cols)
	      (empty-sosofo)
	      (make sequence
		(simplelist-entry rownum colnum majororder rows cols members)
		(colloop (+ colnum 1)))))))

(define (simplelist-entry rownum colnum majororder rows cols members)
  (let ((membernum (if (equal? majororder 'row)
		       (+ (* (- rownum 1) cols) colnum)
		       (+ (* (- colnum 1) rows) rownum)))
	(attlist   (if %simplelist-column-width%
		       (list (list "WIDTH" %simplelist-column-width%))
		       '())))
    (let loop ((nl members) (count membernum))
      (if (<= count 1)
	  (make element gi: "TD"
		attributes: attlist
		(if (node-list-empty? nl)
		    (make entity-ref name: "nbsp")
		    (process-node-list (node-list-first nl))))
	  (loop (node-list-rest nl) (- count 1))))))

(element simplelist
  (let ((type (attribute-string "type"))
	(cols (if (attribute-string "columns")
		  (if (> (string->number (attribute-string "columns")) 0)
		      (string->number (attribute-string "columns"))
		      1)
		  1))
	(members (select-elements (children (current-node)) (normalize "member"))))
    (cond
       ((equal? type (normalize "inline")) 
	(process-children))
       ((equal? type (normalize "vert"))
	(simplelist-table 'column cols members))
       ((equal? type (normalize "horiz"))
	(simplelist-table 'row    cols members)))))

(element member
  (let ((type (inherited-attribute-string (normalize "type"))))
    (if (equal? type (normalize "inline"))
	(make sequence
	  (process-children-trim)
	  (if (not (last-sibling?))
	      (literal ", ")
	      (literal "")))
	(process-children))))

(element segmentedlist (process-children))
(element (segmentedlist title) ($lowtitle$ 6))

(element segtitle (empty-sosofo))

(mode seglist-in-seg
  (element segtitle
    (process-children)))

(element seglistitem (process-children))
(element seg 
  (let* ((seg-num (child-number (current-node)))
	 (seglist (parent (parent (current-node))))
	 (segtitle (nth-node (select-elements 
			 (descendants seglist) (normalize "segtitle")) seg-num)))

    ;; Note: segtitle is only going to be the right thing in a well formed
    ;; SegmentedList.  If there are too many Segs or too few SegTitles,
    ;; you'll get something odd...maybe an error

    (with-mode seglist-in-seg
      (make element gi: "P"
	    (make element gi: "B"
		  (sosofo-append (process-node-list segtitle))
		  (literal ": "))
	    (process-children)))))

(element calloutlist 
  (let* ((nsep  (gentext-label-title-sep (gi)))
	 (id    (attribute-string (normalize "id")))
	 (titlesosofo (make sequence
			(literal (gentext-element-name (gi)))
			(if (string=? (element-label) "")
			    (literal nsep)
			    (literal " " (element-label) nsep))
			(element-title-sosofo))))
    (make element gi: "DIV"
	  attributes: (list
		       (list "CLASS" (gi)))
	  (if (node-list-empty? 
	       (select-elements (children (current-node)) (normalize "title")))
	      (empty-sosofo)
	      (make element gi: "P"
		    (make element gi: "B"
			  (if id
			      (make sequence
				(make element gi: "A"
				      attributes: (list (list "NAME" id))
				      (empty-sosofo))
				titlesosofo)
			      titlesosofo))))
	  (make element gi: "DL"
		attributes: '(("COMPACT" "COMPACT"))
		(process-children)))))

(element (calloutlist title) (empty-sosofo))

(element callout 
  (process-children))

(element (calloutlist callout)
  (process-children))

(element (calloutlist callout para)
  (let ((footnotes (select-elements (descendants (current-node))
				    (normalize "footnote"))))
    (make sequence
      (if (= (child-number) 1)
	  (let* ((ilevel (length (hierarchical-number-recursive 
				  (normalize "calloutlist"))))
		 (arearefs (inherited-attribute-string (normalize "arearefs")))
		 (idlist (split arearefs)))
	    (make sequence
	      (make element gi: "DT"
		    (let loop ((ids idlist))
		      (if (null? ids)
			  (empty-sosofo)
			  (make sequence
			    ($callout-mark$ (element-with-id (car ids)) #f)
			    (loop (cdr ids))))))
	      (make element gi: "DD"
		    (process-children))))
	  (make element gi: "DD"
		(make element gi: "P"
		      (process-children))))

      (if (or %footnotes-at-end% (node-list-empty? footnotes))
	  (empty-sosofo)
	  (make element gi: "BLOCKQUOTE"
		attributes: (list
			     (list "CLASS" "FOOTNOTES"))
		(with-mode footnote-mode
		  (process-node-list footnotes)))))))
