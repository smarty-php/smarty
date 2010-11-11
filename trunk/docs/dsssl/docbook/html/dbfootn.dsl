;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ======================================================================
;; Handle footnotes in body text

(element footnote ;; A footnote inserts a reference to itself
  (let ((id (if (attribute-string (normalize "id"))
		(attribute-string (normalize "id"))
		(generate-anchor))))
    (make element gi: "A"
	  attributes: (list
		       (list "NAME" id)
		       (list "HREF" (string-append "#FTN." id)))
	  ($footnote-literal$ (current-node)))))

(element footnoteref
  (let* ((target   (element-with-id (attribute-string (normalize "linkend"))))
	 (id       (if (attribute-string (normalize "id") target)
		       (attribute-string (normalize "id") target)
		       (generate-anchor target)))
	 (curdepth (directory-depth (html-file (current-node))))
	 (entfile  (html-file target))
	 ;; can't use (href-to) here because we tinker with the fragid
	 (href     (if nochunks
		       (string-append "#FTN." id)
		       (string-append (copy-string "../" curdepth)
				      entfile "#FTN." id))))
    (make element gi: "A"
	  attributes: (list
		       (list "HREF" href))
	  ($footnote-literal$ target))))

(define (count-footnote? footnote)
  ;; don't count footnotes in comments (unless you're showing comments)
  ;; or footnotes in tables which are handled locally in the table
  (if (or (and (has-ancestor-member? footnote (list (normalize "comment")))
	       (not %show-comments%))
	  (has-ancestor-member? footnote (list (normalize "tgroup"))))
      #f
      #t))

(define ($chunk-footnote-number$ footnote)
  ;; This is more complex than it at first appears because footnotes 
  ;; can be in Comments which may be suppressed.
  (let* ((footnotes (select-elements
		     (descendants (chunk-parent footnote))
		     (normalize "footnote"))))
    (let loop ((nl footnotes) (num 1))
      (if (node-list-empty? nl)
	  0
	  (if (node-list=? (node-list-first nl) footnote)
	      num
	      (if (count-footnote? (node-list-first nl))
		  (loop (node-list-rest nl) (+ num 1))
		  (loop (node-list-rest nl) num)))))))

(define ($footnote-literal$ node)
  (make element gi: "SPAN"
        attributes: (list
                     (list "CLASS" "footnote"))
        (literal
         (string-append
          "[" ($footnote-number$ node) "]"))))

(define ($table-footnote-number$ footnote)
  (let* ((chunk (ancestor (normalize "tgroup") footnote))
	 (footnotes (select-elements (descendants chunk) (normalize "footnote"))))
    (let loop ((nl footnotes) (num 1))
      (if (node-list-empty? nl)
	  0
	  (if (node-list=? footnote (node-list-first nl))
	      num
	      (loop (node-list-rest nl)
		    (+ num 1)))))))

(define ($footnote-number$ footnote)
  (if (node-list-empty? (ancestor (normalize "tgroup") footnote))
      (format-number ($chunk-footnote-number$ footnote) "1")
      (format-number ($table-footnote-number$ footnote) "a")))

(mode footnote-mode
  (element footnote
    (process-children))

  (element (footnote para)
    (let ((id (if (attribute-string (normalize "id") (parent (current-node)))
		  (attribute-string (normalize "id") (parent (current-node)))
		  (generate-anchor (parent (current-node))))))
      (make element gi: "P"
	    (if (= (child-number) 1)
		(make sequence
		  (make element gi: "A"
			attributes: (list
				     (list "NAME" (string-append "FTN." id))
				     (list "HREF" (href-to (parent (current-node)))))
			($footnote-literal$ (parent (current-node))))
		  (literal " "))
		(literal ""))
	    (process-children))))
)

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

(define (make-endnotes #!optional (node (current-node)))
  (if %footnotes-at-end%
      (let* ((allfootnotes   (select-elements (descendants node) 
					      (normalize "footnote")))
	     (allntfootnotes (non-table-footnotes allfootnotes))
	     (this-chunk     (chunk-parent node))
	     (chunkfootnotes (let loop ((fn allntfootnotes) 
					(chunkfn (empty-node-list)))
			       (if (node-list-empty? fn)
				   chunkfn
				   (if (node-list=? this-chunk
						    (chunk-parent
						     (node-list-first fn)))
				       (loop (node-list-rest fn)
					     (node-list chunkfn 
							(node-list-first fn)))
				       (loop (node-list-rest fn)
					     chunkfn)))))
	     (footnotes      (let loop ((nl chunkfootnotes)
					(fnlist (empty-node-list)))
			       (if (node-list-empty? nl)
				   fnlist
				   (if (count-footnote? (node-list-first nl))
				       (loop (node-list-rest nl) 
					     (node-list fnlist 
							(node-list-first nl)))
				       (loop (node-list-rest nl)
					     fnlist))))))
	(if (node-list-empty? footnotes) 
	    (empty-sosofo)
	    (if (and #f
		     ;; there was a time when make-endnotes was called in
		     ;; more places, and this code prevented footnotes from
		     ;; being output more than once. now that it's only 
		     ;; called in footer-navigation, this code isn't necessary
		     ;; and does the wrong thing if -V nochunks is specified.
		     (or (equal? (gi node) (normalize "reference"))
			 (equal? (gi node) (normalize "part"))
			 (equal? (gi node) (normalize "set"))
			 (equal? (gi node) (normalize "book"))))
		(empty-sosofo) ;; Each RefEntry/Component does its own...
		(make sequence
		  (make-endnote-header)
		  (make element gi: "TABLE"
			attributes: '(("BORDER" "0")
				      ("CLASS" "FOOTNOTES")
				      ("WIDTH" "100%"))
			(with-mode endnote-mode
			  (process-node-list footnotes)))))))
      (empty-sosofo)))

(define (make-endnote-header)
  (let ((headsize (if (equal? (gi) (normalize "refentry")) "H2" "H3")))
    (make element gi: headsize
	  attributes: '(("CLASS" "FOOTNOTES"))
	  (literal (gentext-endnotes)))))

(mode endnote-mode
  (element footnote
    (let ((id (if (attribute-string (normalize "id") (current-node))
		  (attribute-string (normalize "id") (current-node))
		  (generate-anchor (current-node)))))
      (make sequence
	(make element gi: "TR"
	      (make element gi: "TD"
		    attributes: '(("ALIGN" "LEFT")
				  ("VALIGN" "TOP")
				  ("WIDTH" "5%"))
		    (make element gi: "A"
			  attributes: (list
				       (list "NAME" (string-append "FTN." id))
				       (list "HREF" (href-to (current-node))))
                          ($footnote-literal$ (current-node))))
	      (make element gi: "TD"
		    attributes: '(("ALIGN" "LEFT")
				  ("VALIGN" "TOP")
				  ("WIDTH" "95%"))
		    (process-children))))))
)

;; ======================================================================
;; Handle table footnotes

(define (table-footnote-number footnote)
  (format-number (component-child-number footnote 
					 (list (normalize "table") 
					       (normalize "informaltable")))
		 "a"))

(element (entry para footnote)
  (make element gi: "SUP"
	(literal (table-footnote-number (current-node)))))

(define (make-table-endnote-header)
  (make sequence
    (literal (gentext-table-endnotes))
    (make empty-element gi: "BR")))

(define (make-table-endnotes)
  (let* ((footnotes (select-elements (descendants (current-node)) 
				     (normalize "footnote")))
	 (tgroup (ancestor-member (current-node) (list (normalize "tgroup"))))
	 (cols   (string->number (attribute-string (normalize "cols") tgroup))))
    (if (node-list-empty? footnotes) 
	(empty-sosofo)
	(make element gi: "TR"
	  (make element gi: "TD"
		attributes: (list 
			     (list "COLSPAN" (number->string cols)))
		(make-table-endnote-header)
		(with-mode table-footnote-mode
		  (process-node-list footnotes)))))))

(mode table-footnote-mode
  (element footnote
    (process-children))

  (element (footnote para)
    (let* ((target (parent (current-node)))
	   (fnnum (table-footnote-number target))
	   (idstr (if (attribute-string (normalize "id") target)
		      (attribute-string (normalize "id") target)
		      (generate-anchor target))))
      (make sequence
	(if (= (child-number) 1)
	    (make element gi: "A"
		  attributes: (list (list "NAME" (string-append "FTN." idstr)))
		  (literal fnnum 
			   (gentext-label-title-sep (normalize "footnote"))))
	    (empty-sosofo))
	(process-children)
	(make empty-element gi: "BR")))))

