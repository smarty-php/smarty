;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(element revhistory ($book-revhistory$))

(element highlights ($block-container$))

(element (para blockquote)
  (let* ((attrib       (select-elements (children (current-node))
					(normalize "attribution")))
	 (paras        (node-list-filter-by-not-gi
			(children (current-node))
			(list (normalize "attribution")))))
    (make sequence
      (make paragraph
	first-line-start-indent: 0pt ;; workaround a bug/feature?
	;; W/o the preceding line, the first-line-start-indent of the enclosing
	;; paragraph will apply to the paragraphs in this blockquote which is
	;; probably not wanted..
	font-size: (* %bf-size% %smaller-size-factor%)
	line-spacing: (* %bf-size% %line-spacing-factor%
			 %smaller-size-factor%)
	space-before: %para-sep%
	start-indent: (+ (inherited-start-indent) 1em)
	end-indent: 1em
	(process-node-list paras))
      (if (node-list-empty? attrib)
	  (empty-sosofo)
	  (make paragraph
	    font-size: (* %bf-size% %smaller-size-factor%)
	    line-spacing: (* %bf-size% %line-spacing-factor%
			   %smaller-size-factor%)
	    space-before: 0pt
	    end-indent: 1em
	    quadding: 'end
	    (make sequence
	      (literal "\em-dash;")
	      (process-node-list attrib)))))))

(element blockquote
  (let* ((attrib       (select-elements (children (current-node))
					(normalize "attribution")))
	 (paras        (node-list-filter-by-not-gi
			(children (current-node))
			(list (normalize "attribution")))))
    (make sequence
      (make paragraph
	font-size: (* %bf-size% %smaller-size-factor%)
	line-spacing: (* %bf-size% %line-spacing-factor%
			 %smaller-size-factor%)
	space-before: %para-sep%
	start-indent: (+ (inherited-start-indent) 1em)
	end-indent: 1em
	(process-node-list paras))
      (if (node-list-empty? attrib)
	  (empty-sosofo)
	  (make paragraph
	    font-size: (* %bf-size% %smaller-size-factor%)
	    line-spacing: (* %bf-size% %line-spacing-factor%
			     %smaller-size-factor%)
	    space-before: 0pt
	    end-indent: 1em
	    quadding: 'end
	    (make sequence
	      (literal "\em-dash;")
	      (process-node-list attrib)))))))

(element (blockquote para)
  (if (absolute-last-sibling? (current-node))
      (make paragraph
	space-before: %para-sep%
	space-after: 0pt
 	quadding: %default-quadding%
	(process-children-trim))
      ($paragraph$)))

(element epigraph
  (let* ((addln-indent (* %text-width% 0.55))
	 (attrib       (select-elements (children (current-node))
					(normalize "attribution")))
	 (paras        (node-list-filter-by-not-gi
			(children (current-node))
			(list (normalize "attribution")))))
    (make display-group
      start-indent: (+ %body-start-indent% addln-indent)
      font-posture: 'italic
      (process-node-list paras)
      (if (node-list-empty? attrib)
	  (empty-sosofo)
	  (make paragraph
	    space-before: 0pt
	    quadding: 'end
	    (process-node-list attrib))))))

(element attribution
  ($charseq$))

(element (epigraph para)
  (if (absolute-last-sibling? (current-node))
      (make paragraph
	space-before: %para-sep%
	space-after: 0pt
 	quadding: %default-quadding%
	(process-children-trim))
      ($paragraph$)))

(element para ($paragraph$))
(element simpara ($paragraph$))

(element formalpara ($para-container$))

(element (formalpara title) ($runinhead$))
(element (formalpara para) (make sequence (process-children)))

(element sidebar 
  (make box
    display?: #t
    box-type: 'border
    line-thickness: 1pt
    start-indent: (inherited-start-indent)
    end-indent: (inherited-end-indent)
    (if (node-list-empty? (select-elements (children (current-node))
					   (normalize "title")))
	(make display-group
	  start-indent: 2pt
	  end-indent: 2pt
	  space-before: %block-sep%
	  space-after: %block-sep%
	  (process-children))
	(make display-group
	  start-indent: 2pt
	  end-indent: 2pt
	  space-before: 0pt
	  space-after: %block-sep%
	  (make sequence
	    (let* ((object (current-node))
		   (title  (select-elements (children object) 
					    (normalize "title")))
		   (nsep   (gentext-label-title-sep (gi object))))
	      (make paragraph
		font-weight: 'bold
		space-before: %block-sep%
		space-after: %para-sep%
		keep-with-next?: #t
		(literal (gentext-element-name object))
		(if (string=? (element-label object) "")
		    (literal nsep)
		    (literal " " (element-label object) nsep))
		(process-node-list (children title))))
	    (process-children))))))

(element (sidebar title) (empty-sosofo))

(element abstract 
  (make display-group
    space-before: %block-sep%
    space-after: %block-sep%
    start-indent: %body-start-indent%
    (process-children)))

(element authorblurb ($block-container$))

(element ackno ($paragraph$))

(define ($inline-object$)
  (process-children))

(define ($informal-object$ #!optional (rule-before? #f) (rule-after? #f))
  (make display-group
    start-indent: (+ %block-start-indent% (inherited-start-indent))
    space-before: %block-sep%
    space-after: %block-sep%
    (if rule-before?
	(make rule
	  orientation: 'horizontal
	  line-thickness: %object-rule-thickness%
	  display-alignment: 'center
	  space-after: (/ %block-sep% 2)
	  keep-with-next?: #t)
	(empty-sosofo))
    (process-children)
    (if rule-after?
	(make rule
	  orientation: 'horizontal
	  line-thickness: %object-rule-thickness%
	  display-alignment: 'center
	  space-before: (/ %block-sep% 2)
	  keep-with-previous?: #t)
	(empty-sosofo))))

(define (object-title-after #!optional (node (current-node))) 
  (if (member (gi node) ($object-titles-after$))
      #t
      #f))

(define (float-object node)
  ;; you could redefine this to make only figures float, or only tables,
  ;; or whatever...
  #t)

(define (named-formal-objects)
  (list (normalize "figure")
	(normalize "table")
	(normalize "example")
	(normalize "equation")))

(define ($formal-object$ #!optional (rule-before? #f) (rule-after? #f))
  (let* ((object-sosofo (make paragraph
			  space-before: 0pt
			  space-after: 0pt
			  start-indent: (+ %block-start-indent% 
					   (inherited-start-indent))
			  keep-with-next?: (object-title-after)
			  (process-children)))
	 (title-sosofo  (with-mode formal-object-title-mode
			  (process-node-list 
			   (select-elements (children (current-node))
					    (normalize "title")))))
	 (sosofo        (make display-group
			  space-before: %block-sep%
			  space-after: %block-sep%
			  (if rule-before?
			      (make rule
				orientation: 'horizontal
				line-thickness: %object-rule-thickness%
				display-alignment: 'center
				space-after: (/ %block-sep% 2)
				keep-with-next?: #t)
			      (empty-sosofo))
			  (if (object-title-after)
			    (make sequence
			      object-sosofo
			      title-sosofo)
			    (make sequence
			      title-sosofo
			      object-sosofo))
			  (if rule-after?
			      (make rule
				orientation: 'horizontal
				line-thickness: %object-rule-thickness%
				display-alignment: 'center
				space-before: (/ %block-sep% 2)
				keep-with-previous?: #t)
			      (empty-sosofo)))))
    (if (and (equal? (print-backend) 'tex)
	     formal-object-float
	     (float-object (current-node)))
	(make page-float
	  sosofo)
	sosofo)))

(define ($semiformal-object$)
  (if (node-list-empty? (select-elements (children (current-node))
					 (normalize "title")))
      ($informal-object$)
      ($formal-object$)))

(mode formal-object-title-mode
  (element title
    (let* ((object (parent (current-node)))
	   (nsep   (gentext-label-title-sep (gi object))))
      (make paragraph
	font-weight: 'bold
	space-before: (if (object-title-after (parent (current-node)))
			  %para-sep%
			  0pt)
	space-after: (if (object-title-after (parent (current-node)))
			 0pt
			 %para-sep%)
	start-indent: (+ %block-start-indent% (inherited-start-indent))
	keep-with-next?: (not (object-title-after (parent (current-node))))
	(if (member (gi object) (named-formal-objects))
	    (make sequence
	      (literal (gentext-element-name object))
	      (if (string=? (element-label object) "")
		  (literal nsep)
		  (literal " " (element-label object) nsep)))
	    (empty-sosofo))
	(process-children))))
)

(element example
  ($formal-object$ %example-rules% %example-rules%))

(element (example title) (empty-sosofo)) ; don't show caption below example

(element informalexample
  ($informal-object$ %informalexample-rules% %informalexample-rules%))

(element (figure title) (empty-sosofo)) ; don't show caption below figure

(element figure
  ;; FIXME: this is a bit crude...
  (let* ((mediaobj (select-elements (children (current-node))
				    (normalize "mediaobject")))
	 (imageobj (select-elements (children mediaobj)
				    (normalize "imageobject")))
	 (image    (select-elements (children imageobj)
				    (normalize "imagedata")))
	 (graphic  (select-elements (children (current-node))
				    (normalize "graphic")))
	 (align    (if (node-list-empty? image)
		       (if (node-list-empty? graphic)
			   #f
			   (attribute-string (normalize "align")
					     (node-list-first graphic)))
		       (attribute-string (normalize "align") (node-list-first image))))
	 (dalign  (cond ((equal? align (normalize "center"))
			 'center)
			((equal? align (normalize "right"))
			 'end)
			(else
			 'start))))
    (if align
	(make display-group
	  quadding: dalign
	  ($formal-object$ %figure-rules% %figure-rules%))
	($formal-object$ %figure-rules% %figure-rules%))))

(element informaltable 
  ($informal-object$ %informaltable-rules% %informaltable-rules%))

(element table 
  ;; can't be a "formal-object" because it requires special handling for
  ;; the PGWIDE attribute
  (let* ((nsep   (gentext-label-title-sep (gi)))
	 (pgwide (attribute-string (normalize "pgwide")))
	 (indent (lambda () (if (not (equal? pgwide "1"))
				(+ %block-start-indent% 
				   (inherited-start-indent))
				%cals-pgwide-start-indent%)))
	 (rule-before? %table-rules%)
	 (rule-after? %table-rules%)
	 (title-sosofo (make paragraph
			 font-weight: 'bold
			 space-before: (if (object-title-after)
					   %para-sep%
					   0pt)
			 space-after: (if (object-title-after)
					  0pt
					  %para-sep%)
			 start-indent: (indent)
			  keep-with-next?: (not (object-title-after))
			  (literal (gentext-element-name (current-node)))
			  (if (string=? (element-label) "")
			      (literal nsep)
			      (literal " " (element-label) nsep))
			  (element-title-sosofo)))
	  (table-sosofo (make display-group
			  font-weight: 'bold
			  space-before: 0pt
			  space-after: 0pt
			  start-indent: (indent)
			  keep-with-next?: (object-title-after)
			  (process-children)))
	  (table (make display-group
		   start-indent: (+ %block-start-indent%
				    (inherited-start-indent))
		   space-before: %block-sep%
		   space-after: %block-sep%
		   (if rule-before?
		       (make rule
			 orientation: 'horizontal
			 line-thickness: %object-rule-thickness%
			 display-alignment: 'center
			 space-after: (/ %block-sep% 2)
			 keep-with-next?: #t)
		       (empty-sosofo))
		   (if (object-title-after)
		       (make sequence
			 table-sosofo
			 title-sosofo)
		       (make sequence
			 title-sosofo
			 table-sosofo))
		   (if rule-after?
		       (make rule
			 orientation: 'horizontal
			 line-thickness: %object-rule-thickness%
			 display-alignment: 'center
			 space-before: (/ %block-sep% 2)
			 keep-with-previous?: #t)
		       (empty-sosofo)))))
    (if (and (equal? (print-backend) 'tex)
	     formal-object-float
	     (float-object (current-node)))
	(make page-float
	  table)
	table)))

(element (table title) (empty-sosofo))

(element comment
  (if %show-comments%
      (make paragraph
	start-indent: 0pt
	first-line-start-indent: -10pt
	font-posture: 'italic
	font-size: (* (inherited-font-size) 0.9)
	(make sequence
	  (make line-field 
	    field-width: 10pt
	    quadding: 'end
	    (literal "*"))
	  (process-children)))
      (empty-sosofo)))

;; In DocBook V4.0 comment became remark
(element remark
  (if %show-comments%
      (make paragraph
	start-indent: 0pt
	first-line-start-indent: -10pt
	font-posture: 'italic
	font-size: (* (inherited-font-size) 0.9)
	(make sequence
	  (make line-field 
	    field-width: 10pt
	    quadding: 'end
	    (literal "*"))
	  (process-children)))
      (empty-sosofo)))

;; ======================================================================
;; Handle footnotes in the body...

(define %footnote-field-width% 1.6em)
(define %footnote-number-restarts% #t)
(define %footnote-endnote-break% #f)

(define (count-footnote? footnote)
  ;; don't count footnotes in comments (unless you're showing comments)
  ;; or footnotes in tables which are handled locally in the table
  (if (or (and (has-ancestor-member? footnote (list (normalize "comment")))
	       (not %show-comments%))
	  (has-ancestor-member? footnote (list (normalize "tgroup"))))
      #f
      #t))

(define (footnote-number footnote)
  ;; This is more complex than it at first appears because footnotes 
  ;; can be in Comments which may be suppressed.
  (let* ((root-list (if %footnote-number-restarts%
			    (component-element-list)
			    (list (normalize "book"))))
	 (footnotes (if %footnote-ulinks%
			(component-list-descendant-node-list
			 footnote
			 (list (normalize "ulink") (normalize "footnote"))
			 root-list)
			(component-descendant-node-list
			 footnote
			 root-list)))
	 (fn-number (let loop ((nl footnotes) (num 1))
		      (if (node-list-empty? nl)
			  0
			  (if (node-list=? (node-list-first nl) footnote)
			      num
			      (if (count-footnote? (node-list-first nl))
				  (loop (node-list-rest nl) (+ num 1))
				  (loop (node-list-rest nl) num)))))))
    (format-number fn-number "1")))
	      
(element footnote 
  (if (and (equal? (print-backend) 'tex) bop-footnotes)
      (make sequence
	($ss-seq$ + (literal (footnote-number (current-node))))
	(make page-footnote (process-children)))
      ($ss-seq$ + (literal (footnote-number (current-node))))))

(element (footnote para)
  ;; Note: this can only get called if the backend is 'tex
  ;; If the backend is anything else, footnote never calls process
  ;; children except in endnote-mode, so this doesn't get called.
  (let ((fnnum  (footnote-number (parent (current-node)))))
    (if (= (child-number) 1)
	(make paragraph
 	  font-family-name: %body-font-family%
	  font-size: (* %footnote-size-factor% %bf-size%)
	  font-posture: 'upright
	  quadding: %default-quadding%
	  line-spacing: (* (* %footnote-size-factor% %bf-size%)
			   %line-spacing-factor%)
	  space-before: %para-sep%
	  space-after: %para-sep%
	  start-indent: %footnote-field-width%
	  first-line-start-indent: (- %footnote-field-width%)
	  (make line-field
	    field-width: %footnote-field-width%
	    (literal fnnum 
		     (gentext-label-title-sep (normalize "footnote"))))
	  (process-children-trim))
	(make paragraph
 	  font-family-name: %body-font-family%
	  font-size: (* %footnote-size-factor% %bf-size%)
	  font-posture: 'upright
	  quadding: %default-quadding%
	  line-spacing: (* (* %footnote-size-factor% %bf-size%)
			   %line-spacing-factor%)
	  space-before: %para-sep%
	  space-after: %para-sep%
	  start-indent: %footnote-field-width%
	  (process-children-trim)))))

(define (non-table-footnotes footnotenl)
  (let loop ((nl footnotenl) (result (empty-node-list)))
    (if (node-list-empty? nl)
	result
	(if (has-ancestor-member? (node-list-first nl) 
				  (list (normalize "tgroup")))
	    (loop (node-list-rest nl) 
		  result)
	    (loop (node-list-rest nl)
		  (node-list result (node-list-first nl)))))))

(define (make-endnote-header)
  (let ((headsize (if (equal? (gi) (normalize "refentry"))
		      (HSIZE 2)
		      (HSIZE 3)))
	(indent   (lambda () (if (equal? (gi) (normalize "refentry"))
				 %body-start-indent%
				 0pt))))
    (make paragraph
      break-before: %footnote-endnote-break%
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: headsize
      line-spacing: (* headsize %line-spacing-factor%)
      space-before: (* headsize %head-before-factor%)
      space-after: (* headsize %head-after-factor%)
      start-indent: (indent)
      quadding: 'start
      keep-with-next?: #t
      (literal (gentext-endnotes)))))

(define (make-endnotes #!optional (node (current-node)))
  (let* ((allfootnotes (if %footnote-ulinks%
			   (node-list-filter-by-gi
			    (descendants node)
			    (list (normalize "footnote") (normalize "ulink")))
			   (select-elements (descendants node) 
					    (normalize "footnote"))))
	 (footnotes    (let loop ((nl (non-table-footnotes allfootnotes))
				  (fnlist (empty-node-list)))
			 (if (node-list-empty? nl)
			     fnlist
			     (if (count-footnote? (node-list-first nl))
				 (loop (node-list-rest nl) 
				       (node-list fnlist (node-list-first nl)))
				 (loop (node-list-rest nl)
				       fnlist))))))
    (if (or (node-list-empty? footnotes) 
	    (and (equal? (print-backend) 'tex)
		 bop-footnotes))
	(empty-sosofo)
	(if (or (equal? (gi node) (normalize "reference"))
		(equal? (gi node) (normalize "part")))
	    (empty-sosofo) ;; Each RefEntry/Component does its own...
	    (make sequence
	      (make-endnote-header)
	      (with-mode endnote-mode
		(process-node-list footnotes)))))))

(mode endnote-mode
  (element footnote
    (make sequence
      start-indent: %body-start-indent%
      (process-children)))

  (element (footnote para)
    (let ((fnnum  (footnote-number (parent (current-node)))))
      (if (= (child-number) 1)
	  (make paragraph
; I'm not sure this really makes sense in the endnote case...
;	    font-size: (* %footnote-size-factor% %bf-size%)
;	    line-spacing: (* (* %footnote-size-factor% %bf-size%)
;			     %line-spacing-factor%)
	    space-before: %para-sep%
	    start-indent: (+ (inherited-start-indent) %footnote-field-width%)
	    first-line-start-indent: (- %footnote-field-width%)
	    (make line-field
	      field-width: %footnote-field-width%
	      (literal fnnum 
		       (gentext-label-title-sep (normalize "footnote"))))
	    (process-children-trim))
	  (make paragraph
	    font-size: (* %footnote-size-factor% %bf-size%)
	    line-spacing: (* (* %footnote-size-factor% %bf-size%)
			     %line-spacing-factor%)
	    start-indent: (+ (inherited-start-indent) %footnote-field-width%)
	    space-before: %para-sep%
	    (process-children-trim)))))

  (element ulink
    (if %footnote-ulinks%
	(let ((fnnum  (footnote-number (current-node))))
	  (make paragraph
;	    font-size: (* %footnote-size-factor% %bf-size%)
;	    line-spacing: (* (* %footnote-size-factor% %bf-size%)
;			     %line-spacing-factor%)
	    space-before: %para-sep%
	    start-indent: (+ (inherited-start-indent) %footnote-field-width%)
	    first-line-start-indent: (- %footnote-field-width%)
	    (make line-field
	      field-width: %footnote-field-width%
	      (literal fnnum 
		       (gentext-label-title-sep (normalize "footnote"))))
	    (literal (attribute-string "url"))))
	(next-match))))

;; ======================================================================
;; Handle table footnotes

(define (table-footnote-number footnote)
  (format-number (component-child-number footnote 
					 ($table-element-list$)) "a"))

(element (entry footnote)
  ($ss-seq$ + (literal (table-footnote-number (current-node)))))

(element (entry para footnote)
  ($ss-seq$ + (literal (table-footnote-number (current-node)))))

(define (make-table-endnote-header)
  (make paragraph
    font-family-name: %body-font-family%
    font-weight: 'medium
    font-size: %bf-size%
    start-indent: 0pt
    quadding: 'start
    (literal (gentext-table-endnotes))))

(define (make-table-endnotes)
  (let* ((footnotes (select-elements (descendants (current-node)) 
				     (normalize "footnote")))
	 (headsize (HSIZE 3))
	 (tgroup (ancestor-member (current-node) (list (normalize "tgroup"))))
	 (cols   (string->number (attribute-string (normalize "cols") tgroup))))
    (if (node-list-empty? footnotes) 
	(empty-sosofo)
	(make table-row
	  (make table-cell
	    n-columns-spanned: cols
	    cell-before-row-margin: %cals-cell-before-row-margin%
	    cell-after-row-margin: %cals-cell-after-row-margin%
	    cell-before-column-margin: %cals-cell-before-column-margin%
	    cell-after-column-margin: %cals-cell-after-column-margin%
	    start-indent: %cals-cell-content-start-indent%
	    end-indent: %cals-cell-content-end-indent%
	    (make-table-endnote-header)
	    (with-mode table-footnote-mode
	      (process-node-list footnotes)))))))

(mode table-footnote-mode
  (element footnote
    (make display-group
      font-family-name: %body-font-family%
      font-weight: 'medium
      font-size: %bf-size%
      start-indent: 0pt
      quadding: 'start
      (process-children)))

  (element (footnote para)
    (let ((fnnum (table-footnote-number (parent (current-node)))))
      (if (= (child-number) 1)
	  (make paragraph
	    start-indent: %footnote-field-width%
	    first-line-start-indent: (- %footnote-field-width%)
	    (make line-field
	      field-width: %footnote-field-width%
	      (literal fnnum 
		       (gentext-label-title-sep (normalize "footnote"))))
	    (process-children-trim))
	  (make paragraph
	    start-indent: %footnote-field-width%
	    (process-children-trim))))))

