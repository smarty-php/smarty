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
    (make sequence
      (if %generate-reference-titlepage%
	  (make sequence
	    (reference-titlepage nl 'recto)
	    (reference-titlepage nl 'verso))
	  (empty-sosofo))

      (if (not (generate-toc-in-front))
	  (process-children)
	  (empty-sosofo))
      
      (if (and %generate-reference-toc%
	       (not %generate-reference-toc-on-titlepage%))
	  (make simple-page-sequence
	    page-n-columns: %page-n-columns%
	    page-number-restart?: #t
	    page-number-format: ($page-number-format$ (normalize "toc"))
	    use: default-text-style
	    left-header:   ($left-header$ (normalize "toc"))
	    center-header: ($center-header$ (normalize "toc"))
	    right-header:  ($right-header$ (normalize "toc"))
	    left-footer:   ($left-footer$ (normalize "toc"))
	    center-footer: ($center-footer$ (normalize "toc"))
	    right-footer:  ($right-footer$ (normalize "toc"))
	    input-whitespace-treatment: 'collapse
	    (build-toc (current-node)
		       (toc-depth (current-node))))
	  (empty-sosofo))

      (if (and (not (node-list-empty? refintro))
	       (not %generate-partintro-on-titlepage%))
	  ($process-partintro$ refintro #t)
	  (empty-sosofo))

      (if (generate-toc-in-front)
	  (if %refentry-new-page%
	      (process-children)
	      (make simple-page-sequence
		page-n-columns: %page-n-columns%
		page-number-format: ($page-number-format$)
		use: default-text-style
		left-header:   ($left-header$)
		center-header: ($center-header$)
		right-header:  ($right-header$)
		left-footer:   ($left-footer$)
		center-footer: ($center-footer$)
		right-footer:  ($right-footer$)
		input-whitespace-treatment: 'collapse
		quadding: %default-quadding%
		(process-children)))
	  (empty-sosofo)))))

;; If each RefEntry begins on a new page, this title is going to wind
;; up on its own page, too, so make it a divtitlepage instead.  Otherwise,
;; just let it be a component title.
(element (reference title) (empty-sosofo))
;;  (if %refentry-new-page%
;;      ($divtitlepage$)
;;      (empty-sosofo)))

(element refentry 
  (make display-group
    keep: %refentry-keep%
    (if (or %refentry-new-page%
	    (node-list=? (current-node) (sgml-root-element)))
	(make simple-page-sequence
	  page-n-columns: %page-n-columns%
	  page-number-format: ($page-number-format$)
	  use: default-text-style
	  left-header:   ($left-header$)
	  center-header: ($center-header$)
	  right-header:  ($right-header$)
	  left-footer:   ($left-footer$)
	  center-footer: ($center-footer$)
	  right-footer:  ($right-footer$)
	  input-whitespace-treatment: 'collapse
	  quadding: %default-quadding%
	  ($refentry-title$)
	  (process-children))
	(make sequence
	  ($refentry-title$)
	  ($block-container$)))
    (make-endnotes)))

(define ($refentry-title$)
  (let* ((refmeta       (select-elements (children (current-node))
					 (normalize "refmeta")))
	 (refentrytitle (select-elements (children refmeta)
					 (normalize "refentrytitle")))
	 (refnamediv    (select-elements (children (current-node))
					 (normalize "refnamediv")))
	 (refdescriptor (select-elements (children refnamediv)
					 (normalize "refdescriptor")))
	 (refname       (select-elements (children refnamediv)
					 (normalize "refname")))
	 (title         (if (node-list-empty? refentrytitle)
			    (if (node-list-empty? refdescriptor)
				(node-list-first refname)
				refdescriptor)
			    refentrytitle))
	 (slevel (SECTLEVEL)) ;; the true level in the section hierarchy
	 (hlevel (if (> slevel 2) 2 slevel)) ;; limit to sect2 equiv.
	 (hs (HSIZE (- 4 hlevel))))
    (make paragraph
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: hs
      line-spacing: (* hs %line-spacing-factor%)
      space-before: (* hs %head-before-factor%)
      space-after: (* hs %head-after-factor%)
      start-indent: %body-start-indent%
      first-line-start-indent: (- %body-start-indent%)
      quadding: 'start
      heading-level: (if %generate-heading-level% 2 0)
      keep-with-next?: #t
      (process-node-list (children title)))))
;; nwalsh, this is wrong, 29 July 1999
;      (if %refentry-function%
;	  (sosofo-append
;	   (literal "\no-break-space;")
;	   (process-first-descendant (normalize "manvolnum")))
;	  (empty-sosofo)))))

(element refmeta (empty-sosofo)) ;; handled by $refentry-title$

(element manvolnum 
  (if %refentry-xref-manvolnum%
      (sosofo-append
       (literal "(")
       (process-children)
       (literal ")"))
      (empty-sosofo)))

(element refmiscinfo (empty-sosofo))

(element refentrytitle ($charseq$))

(element refnamediv
  (make paragraph
	space-before: %para-sep%
	start-indent: %body-start-indent%
	quadding: 'start
	(process-children)))

(element refname
  (make sequence
    (if (and %refentry-generate-name% (first-sibling? (current-node)))
 	($lowtitlewithsosofo$ 1 3 (literal (gentext-element-name 
					    (current-node))))
 	(empty-sosofo))
    (make sequence
	  font-weight: 'medium
	  font-family-name: %refentry-name-font-family%
	  (process-children)
 	  (if (last-sibling? (current-node))
 	      (empty-sosofo)
	      (literal (gentext-intra-label-sep (gi (current-node))))))))

(element refpurpose
  (make sequence
        font-family-name: %body-font-family%
    (make sequence
      (literal " \em-dash ")
      (process-children))
    (make paragraph-break)))
	
(element refdescriptor (empty-sosofo))

(element refclass
  (let ((role (attribute-string "role")))
    (make paragraph
	  space-before: %para-sep%
	  start-indent: %body-start-indent%
	  quadding: 'start
	  (make sequence
		font-weight: 'bold
		(literal
		  (if role
		      (string-append role ": ")
		      "")))
	  (process-children-trim))))

(element refsynopsisdiv ($section$))

(element (refsynopsisdiv title) (empty-sosofo))

(element refsect1 ($section$))
(element (refsect1 title) (empty-sosofo))
(element refsect2 ($section$))
(element (refsect2 title) (empty-sosofo))
(element refsect3 ($section$))
(element (refsect3 title) (empty-sosofo))


