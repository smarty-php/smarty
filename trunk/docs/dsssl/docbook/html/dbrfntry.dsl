;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; =========================== REFERENCE PAGES ==========================

;;(element reference ($component$))

(element reference
  (let* ((refinfo  (select-elements (children (current-node)) 
				    (normalize "docinfo")))
	 (refintro (select-elements (children (current-node)) 
				    (normalize "partintro")))
	 (nl       (titlepage-info-elements 
		    (current-node)
		    refinfo
		    (if %generate-partintro-on-titlepage%
			refintro
			(empty-node-list)))))
    (html-document 
     (with-mode head-title-mode 
       (literal (element-title-string (current-node))))
     (make sequence
       (make element gi: "DIV"
	     attributes: (list (list "CLASS" (gi)))

	     (make element gi: "A"
		   attributes: (list (list "NAME" (element-id)))
		   (empty-sosofo))

	     (if %generate-reference-titlepage%
		 (make sequence
		   (reference-titlepage nl 'recto)
		   (reference-titlepage nl 'verso))
		 (empty-sosofo))

	     (if (not (generate-toc-in-front))
		 (process-children)
		 (empty-sosofo))
	     
	     (if (and (not (node-list-empty? refintro))
		      (not %generate-partintro-on-titlepage%))
		 ($process-partintro$ refintro)
		 (empty-sosofo))
	     
	     (if (and %generate-reference-toc%
		      (not %generate-reference-toc-on-titlepage%))
		 (make sequence
		   (build-toc (current-node)
			      (toc-depth (current-node))))
		 (empty-sosofo))
	     
	     (if (generate-toc-in-front)
		 (process-children)
		 (empty-sosofo)))))))

;; If each RefEntry begins on a new page, this title is going to wind
;; up on its own page, too, so make it a divtitlepage instead.  Otherwise,
;; just let it be a component title.
(element (reference title) 
  (empty-sosofo))

(mode refentry-head-title-mode
  (default (process-children))

  (element refnamediv
    (let* ((refdesc (select-elements (children (current-node)) 
				     (normalize "refdescriptor")))
	   (refname (select-elements (children (current-node)) 
				     (normalize "refname")))
	   (title   (if (node-list-empty? refdesc)
			(node-list-first refname)
			(node-list-first refdesc))))
      (process-node-list title)))

  (element refdescriptor
    (process-children))

  (element refname
    (process-children))

  (element graphic (empty-sosofo))
  (element inlinegraphic (empty-sosofo)))

(define ($refentry-body$)
  (let ((id (element-id (current-node))))
    (make sequence 
      (make element gi: "H1"
	    (make sequence
	      (make element gi: "A"
		    attributes: (list (list "NAME" id))
		    (empty-sosofo))
	      (element-title-sosofo (current-node))))
      (process-children))))

(element refentry
  (html-document (with-mode refentry-head-title-mode
		   (literal (element-title-string (current-node))))
		 ($refentry-body$)))

(element refmeta (empty-sosofo))

(element manvolnum 
  ;; only called for xrefs and citerefentry
  (if %refentry-xref-manvolnum%
      (sosofo-append
       (literal "(")
       (process-children)
       (literal ")"))
      (empty-sosofo)))

(element refmiscinfo (empty-sosofo))

(element refentrytitle ($charseq$))

(element refnamediv ($block-container$))

(element refname
  (make sequence
    (if (and %refentry-generate-name% (first-sibling? (current-node)))
 	($lowtitlewithsosofo$ 2 (literal (gentext-element-name 
					  (gi (current-node)))))
 	(empty-sosofo))
    (make sequence
	  (process-children)
 	  (if (last-sibling? (current-node))
 	      (empty-sosofo)
	      (literal (gentext-intra-label-sep (gi (current-node))))))))

(element refpurpose
  (make sequence
    (make entity-ref name: "nbsp")
    (literal (dingbat "em-dash"))
    (make entity-ref name: "nbsp")
    (process-children)))
	
(element refdescriptor (empty-sosofo)) ;; TO DO: finish this

(element refclass
  (let ((role (attribute-string (normalize "role"))))
    (make element gi: "P"
	  (make element gi: "B"
		(literal
		 (if role
		     (string-append role ": ")
		     "")))
	  (process-children-trim))))

(element refsynopsisdiv
  (make element gi: "DIV"
	attributes: (list (list "CLASS" (gi)))
	(make element gi: "A"
	      attributes: (list (list "NAME" (element-id)))
	      (empty-sosofo))
	(make element gi: "H2"
	      (element-title-sosofo (current-node)))
	(process-children)))

(element (refsynopsisdiv title) (empty-sosofo))

(element refsect1 ($block-container$))
(element (refsect1 title) ($lowtitle$ 2))
(element refsect2 ($block-container$))
(element (refsect2 title) ($lowtitle$ 3))
(element refsect3 ($block-container$))
(element (refsect3 title) ($lowtitle$ 4))


