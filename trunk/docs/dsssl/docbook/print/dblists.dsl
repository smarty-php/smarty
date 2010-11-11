;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; =============================== LISTS ================================

(define (BULLTREAT bullfcn ilevel override mark)
  (cond
   (override (bullfcn override ilevel))
   (mark (bullfcn mark ilevel))
   (else (bullfcn "bullet" ilevel))))

(define (BULLSTR m lvl)
  (dingbat m))

(define (BULLSHIFT m lvl)
  (let ((md (case-fold-down m)))
    (case md
	  (("bullet") 0.0em)
	  (("box") (if (= lvl 1) 0.0em 0.1em))
	  (("checkbox") (if (= lvl 1) 0.0em 0.1em))
	  (("check") 0.0em)
	  (("checkedbox") 0.0em)
	  (("dash") 0.0em)
	  (("none") 0.0em)
	  (else 0.0em))))

(define (MSIZE m lvl f1 f2)
  (if (= lvl 1)
      (* %bf-size% f1)
      (* %bf-size% f2)))

(define (BULLSIZE m lvl)
  (let ((md (case-fold-down m)))
    (case md
	  (("bullet") (MSIZE m lvl 0.8 0.72))
	  (("box") (MSIZE m lvl 0.9 0.72))
	  (("checkbox") (MSIZE m lvl 0.9 0.72))
	  (("check") (MSIZE m lvl 1.0 1.0))
	  (("checkedbox") (MSIZE m lvl 1.0 1.0))
	  (("dash") (MSIZE m lvl 1.0 1.0))
	  (("none") (MSIZE m lvl 1.0 1.0))
	  (else (MSIZE m lvl 1.0 1.0)))))

(define (OLSTEP) 0.9em)
;;  (case
;;   (modulo (length (hierarchical-number-recursive (normalize "orderedlist"))) 4)
;;	((1) 1.4em)
;;	((2) 1.4em)
;;	((3) 1.4em)
;;	((0) 1.4em)))

(define (ILSTEP) 1.0em)

(define (COSTEP) 1.5pi)

;; Improve spacing on lists, remove extra space before..
;; Suggested by Adam Di Carlo, adam@onshore.com
(define ($list$)
  (make display-group
    start-indent: (if (INBLOCK?)
                      (inherited-start-indent)
                      (+ %block-start-indent% (inherited-start-indent)))
    space-after:  (if (INLIST?) %para-sep% %block-sep%)))

(element itemizedlist ($list$))

(element (itemizedlist title)
  (make paragraph
    use: title-style
    (process-children)))

(define (generic-list-item indent-step line-field)
  (let* ((itemcontent (children (current-node)))
         (first-child (node-list-first itemcontent))
         (spacing (inherited-attribute-string (normalize "spacing"))))
    (make display-group
      start-indent: (+ (inherited-start-indent) indent-step)
      (make paragraph
        use: (cond
              ((equal? (gi first-child) (normalize "programlisting"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "screen"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "synopsis"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "literallayout"))
               linespecific-style)
              ((equal? (gi first-child) (normalize "address"))
               linespecific-style)
              (else
               nop-style))
        space-before: (if (equal? (normalize "compact") spacing)
                          0pt
                          %para-sep%)
        first-line-start-indent: (- indent-step)
        (make sequence
          line-field)
	(with-mode listitem-content-mode
	  (process-node-list first-child)))
      (process-node-list (node-list-rest itemcontent)))))

(define (process-listitem-content)
  (if (absolute-first-sibling?) 
      (make sequence
	(process-node-list (children (current-node))))
      (next-match)))

(mode listitem-content-mode
  (element (listitem programlisting) (process-listitem-content))
  (element (listitem screen) (process-listitem-content))
  (element (listitem synopsis) (process-listitem-content))
  (element (listitem funcsynopsis) (process-listitem-content))
  (element (listitem literallayout) (process-listitem-content))
  (element (listitem address) (process-listitem-content))
  (element (listitem para) (process-listitem-content))
  (element (listitem formalpara) (process-listitem-content))
  (element (listitem simpara) (process-listitem-content))
)

(element (itemizedlist listitem)
  (let ((ilevel (length (hierarchical-number-recursive (normalize "itemizedlist"))))
	(override (inherited-attribute-string (normalize "override")))
	(mark (inherited-attribute-string (normalize "mark"))))
    (generic-list-item
     (ILSTEP)
     (if (or (and override
		  (equal? (normalize override) (normalize "none")))
	     (and (not override)
		  (equal? (normalize mark) (normalize "none"))))
	 (make line-field
	   font-size: (BULLTREAT BULLSIZE ilevel override mark)
	   position-point-shift: (BULLTREAT BULLSHIFT ilevel override mark)
	   field-width: (ILSTEP)
	   (literal "\no-break-space;"))
	 (make line-field
	   font-size: (BULLTREAT BULLSIZE ilevel override mark)
	   position-point-shift: (BULLTREAT BULLSHIFT ilevel override mark)
	   field-width: (ILSTEP)
	   (literal (BULLTREAT BULLSTR ilevel override mark)))))))

(element orderedlist ($list$))

(element (orderedlist title)
  (make paragraph
    use: title-style
    (process-children)))

(element (orderedlist listitem)
  (let* ((listitems (select-elements (children (parent (current-node)))
				     (normalize "listitem")))
	 (itemnumber (orderedlist-listitem-number (current-node)))
	 (displaynum (if (string=? (inherited-attribute-string 
				    (normalize "inheritnum"))
				   (normalize "inherit"))
			 (let loop ((nd (current-node)) (inum ""))
			   (if (node-list-empty? nd)
			       inum
			       (if (and (equal? (gi nd) 
						(normalize "listitem"))
					(equal? (gi (parent nd)) 
						(normalize "orderedlist")))
				   (loop (parent nd)
					 (string-append
					  (number-with-numeration 
					   nd
					   (inherited-attribute-string
					    (normalize "numeration") nd)
					   (orderedlist-listitem-number nd))
					  (if (string=? inum "")
					      ""
					      ".")
					  inum))
				   (loop (parent nd) inum))))
			 (number-with-numeration 
			  (current-node)
			  (inherited-attribute-string (normalize "numeration"))
			  (orderedlist-listitem-number (current-node)))))
	 (listcount (+ (node-list-length listitems) itemnumber))
	 (factor    (cond 
		     ((> listcount 999) 4)
		     ((> listcount 99) 3)
		     ((> listcount 9) 2)
		     (else 2))))
    (generic-list-item
     (* (OLSTEP) factor)
     (make line-field
       field-width: (* (OLSTEP) factor)
       field-align: 'end
       (literal displaynum
		(gentext-label-title-sep (normalize "orderedlist")))))))

(define (number-with-numeration node numeration number)
  (let* ((depth (length (hierarchical-number-recursive (normalize "orderedlist") node)))
	 (rawnum (cond
		  ((equal? numeration (normalize "arabic")) 1)
		  ((equal? numeration (normalize "loweralpha")) 2)
		  ((equal? numeration (normalize "lowerroman")) 3)
		  ((equal? numeration (normalize "upperalpha")) 4)
		  ((equal? numeration (normalize "upperroman")) 0)
		  (else (modulo depth 5))))
	 (num (case rawnum
		((1) (format-number number "1"))
		((2) (format-number number "a"))
		((3) (format-number number "i"))
		((4) (format-number number "A"))
		((0) (format-number number "I")))))
    (if (> depth 5) 
	(string-append "(" num ")")
	num)))
  
(element variablelist
  (let* ((termlength (if (attribute-string (normalize "termlength"))
			 (string->number 
			  (attribute-string (normalize "termlength")))
			 %default-variablelist-termlength%))
	 (maxlen     (if (> termlength %default-variablelist-termlength%)
			    termlength
			    %default-variablelist-termlength%))
	 (too-long?  (variablelist-term-too-long? termlength)))
    (make display-group
      start-indent: (if (INBLOCK?)
			(inherited-start-indent)
			(+ %block-start-indent% (inherited-start-indent)))
      space-before: (if (INLIST?) %para-sep% %block-sep%)
      space-after:  (if (INLIST?) %para-sep% %block-sep%)

      (if (and (or (and termlength (not too-long?))
		   %always-format-variablelist-as-table%)
	       (or %may-format-variablelist-as-table%
		   %always-format-variablelist-as-table%))
	  (make table
	    space-before: (if (INLIST?) %para-sep% %block-sep%)
	    space-after:  (if (INLIST?) %para-sep% %block-sep%)
	    start-indent: (if (INBLOCK?)
			      (inherited-start-indent)
			      (+ %block-start-indent% 
				 (inherited-start-indent)))

;; Calculate the width of the column containing the terms...
;;
;; maxlen       in        (inherited-font-size)     72pt
;;        x ---------- x ----------------------- x ------ = width
;;           12 chars              10pt              in
;;
	    (make table-column
	      column-number: 1
	      width: (* (* (/ maxlen 12) (/ (inherited-font-size) 10pt)) 72pt))
	    (with-mode variablelist-table
	      (process-children)))
	  (process-children)))))

(element varlistentry (process-children))

(element (varlistentry term)
    (make paragraph
	  space-before: (if (first-sibling?)
			    %block-sep%
			    0pt)
	  keep-with-next?: #t
	  first-line-start-indent: 0pt
	  start-indent: (inherited-start-indent)
	  (process-children)))

(element (varlistentry listitem)
  (let ((vle-indent 2em)) ; this ought to be in dbparam!
    (generic-list-item 
     vle-indent
     (make line-field
       field-width: vle-indent
       (literal "\no-break-space;")))))

(mode variablelist-table
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
	    (make table-row
	      cell-before-row-margin: %para-sep%

	      (make table-cell
		column-number: 1
		n-columns-spanned: 2
		n-rows-spanned: 1
		(process-node-list terms)))
	    (make table-row
	      (make table-cell
		column-number: 1
		n-columns-spanned: 1
		n-rows-spanned: 1
		;; where terms would have gone
		(empty-sosofo))
	      (make table-cell
		column-number: 2
		n-columns-spanned: 1
		n-rows-spanned: 1
		(process-node-list listitem))))
	  (make table-row
	    cell-before-row-margin: %para-sep%

	    (make table-cell
	      column-number: 1
	      n-columns-spanned: 1
	      n-rows-spanned: 1
	      (process-node-list terms))
	    (make table-cell
	      column-number: 2
	      n-columns-spanned: 1
	      n-rows-spanned: 1
	      (process-node-list listitem))))))
  
  (element (varlistentry term)
    (make sequence
      (process-children-trim)
      (if (not (last-sibling?))
	  (literal ", ")
	  (empty-sosofo))))

  (element (varlistentry listitem)
    (make display-group
      start-indent: 0pt
      (process-children)))

  ;; Suggested by Nick NICHOLAS, nicholas@uci.edu
  (element (variablelist title)
    (make table-row
      cell-before-row-margin: %para-sep%
      (make table-cell
	column-number: 1
	n-columns-spanned: 2
	n-rows-spanned: 1
	(make paragraph
	  use: title-style
	  start-indent: 0pt
	  (process-children)))))

)

(define (simplelist-table majororder cols members)
  (let* ((termcount (node-list-length members))
	 (rows (quotient (+ termcount (- cols 1)) cols)))
    (make table
      space-before: (if (INLIST?) %para-sep% %block-sep%)
      space-after:  (if (INLIST?) %para-sep% %block-sep%)
      start-indent: (if (INBLOCK?)
			(inherited-start-indent)
			(+ %block-start-indent% (inherited-start-indent)))
      (if %simplelist-column-width%
	  (let colloop ((colnum 1))
	    (if (> colnum cols)
		(empty-sosofo)
		(make sequence
		  (make table-column
		    width: %simplelist-column-width%)
		  (colloop (+ colnum 1)))))
	  (empty-sosofo))
      (let rowloop ((rownum 1))
	(if (> rownum rows)
	    (empty-sosofo)
	    (make sequence
	      (simplelist-row rownum majororder rows cols members)
	      (rowloop (+ rownum 1))))))))

(define (simplelist-row rownum majororder rows cols members)
  (make table-row
    (let colloop ((colnum 1))
      (if (> colnum cols)
	  (empty-sosofo)
	  (make sequence
	    (simplelist-entry rownum colnum majororder rows cols members)
	    (colloop (+ colnum 1)))))))

(define (simplelist-entry rownum colnum majororder rows cols members)
  (let ((membernum (if (equal? majororder 'row)
		       (+ (* (- rownum 1) cols) colnum)
		       (+ (* (- colnum 1) rows) rownum))))
    (let loop ((nl members) (count membernum))
      (if (<= count 1)
	  (make table-cell
	    column-number: colnum
	    n-columns-spanned: 1
	    n-rows-spanned: 1
;; removed to avoid dependency between dblists and dbtable
;;	    cell-before-row-margin: %cals-cell-before-row-margin%
;;	    cell-after-row-margin: %cals-cell-after-row-margin%
;;	    cell-before-column-margin: %cals-cell-before-column-margin%
;;	    cell-after-column-margin: %cals-cell-after-column-margin%
;;	    start-indent: %cals-cell-content-start-indent%
;;	    end-indent: %cals-cell-content-end-indent%
;; is another variable needed to parameterize these settings, or are
;; constants good enough?
	    cell-before-row-margin: 0pt
	    cell-after-row-margin: 0pt
	    cell-before-column-margin: 3pt
	    cell-after-column-margin: 3pt
	    start-indent: 0pt
	    end-indent: 0pt
	    quadding: 'start
	    (if (node-list-empty? nl)
		(literal "\no-break-space;")
		(process-node-list (node-list-first nl))))
	  (loop (node-list-rest nl) (- count 1))))))

(element (entry simplelist) 
  ;; This is to avoid possibly putting tables inside tables, which don't
  ;; work in some backends (e.g. RTF)
  (make paragraph
    (process-children)))

(element (entry simplelist member)
  ;; This is to avoid possibly putting tables inside tables, which don't
  ;; work in some backends (e.g. RTF)
  (let ((type (inherited-attribute-string (normalize "type"))))
    (if (equal? type (normalize "inline"))
	(next-match)
	(make sequence
	  (if (equal? (child-number) 1)
	      (empty-sosofo)
	      (make paragraph-break))
	  (process-children)))))

(element simplelist
  (let ((type (attribute-string (normalize "type")))
	(cols (if (attribute-string (normalize "columns"))
		  (if (> (string->number (attribute-string (normalize "columns"))) 0)
		      (string->number (attribute-string (normalize "columns")))
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
	  (process-children)
	  (if (not (last-sibling?))
	      (literal ", ")
	      (literal "")))
	(make paragraph
	  quadding: 'start
	  (process-children)))))

(element segmentedlist (process-children))
(element (segmentedlist title) ($lowtitle$ 2 4))

(element segtitle (empty-sosofo))
(mode seglist-in-seg
  (element segtitle
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (process-children))))

(element seglistitem ($paragraph$))
(element seg 
  (let* ((seg-num (child-number (current-node)))
	 (seglist (parent (parent (current-node))))
	 (segtitle (nth-node (select-elements 
			 (descendants seglist) (normalize "segtitle")) seg-num)))

    ;; Note: segtitle is only going to be the right thing in a well formed
    ;; SegmentedList.  If there are too many Segs or too few SegTitles,
    ;; you'll get something odd...maybe an error

    (with-mode seglist-in-seg
      (make paragraph
	(make sequence
	  font-family-name: %title-font-family%
	  font-weight: 'bold
	  (sosofo-append (process-node-list segtitle))
	  (literal ": "))
	(process-children)))))

(element calloutlist ($list$))
(element (calloutlist title) ($lowtitle$ 2 4))

(element callout
  (let* ((calloutcontent (children (current-node)))
	 (arearefs (inherited-attribute-string (normalize "arearefs")))
	 (idlist (split arearefs)))
    (make sequence
      start-indent: (+ (inherited-start-indent) (COSTEP))

      (make paragraph
	space-before: %para-sep%
	first-line-start-indent: (- (COSTEP))
	(make line-field
	  field-width: (COSTEP)
	  (let loop ((ids idlist))
	    (if (null? ids)
		(empty-sosofo)
		(make sequence
		  ($callout-mark$ (element-with-id (car ids)))
		  (loop (cdr ids))))))
	(process-node-list (children (node-list-first calloutcontent))))

      (process-node-list (node-list-rest calloutcontent)))))
