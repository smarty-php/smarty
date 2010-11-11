;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================= DIVISIONS ==============================

(element set
  (let* ((setinfo  (select-elements (children (current-node)) 
				    (normalize "setinfo")))
	 (nl       (titlepage-info-elements (current-node) setinfo)))
    (make sequence
      (if %generate-set-titlepage%
	  (make sequence
	    (set-titlepage nl 'recto)
	    (set-titlepage nl 'verso))
	  (empty-sosofo))
      
      (if (not (generate-toc-in-front))
	  (process-children)
	  (empty-sosofo))

      (if %generate-set-toc%
	  (make simple-page-sequence
	    page-n-columns: %page-n-columns%
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
	    
      (if (generate-toc-in-front)
	  (process-children)
	  (empty-sosofo)))))

(element (set title) (empty-sosofo))

(element book 
  (let* ((bookinfo  (select-elements (children (current-node)) 
				     (normalize "bookinfo")))
	 (dedication (select-elements (children (current-node)) 
				      (normalize "dedication")))
	 (nl        (titlepage-info-elements (current-node) bookinfo)))
    (make sequence
      (if %generate-book-titlepage%
	  (make sequence
	    (book-titlepage nl 'recto)
	    (book-titlepage nl 'verso))
	  (empty-sosofo))

      (if (node-list-empty? dedication)
	  (empty-sosofo)
	  (with-mode dedication-page-mode
	    (process-node-list dedication)))

      (if (not (generate-toc-in-front))
	  (process-children)
	  (empty-sosofo))

      (if %generate-book-toc%
	  (make simple-page-sequence
	    page-n-columns: %page-n-columns%
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
	    
      (let loop ((gilist ($generate-book-lot-list$)))
	(if (null? gilist)
	    (empty-sosofo)
	    (if (not (node-list-empty? 
		      (select-elements (descendants (current-node))
				       (car gilist))))
		(make simple-page-sequence
		  page-n-columns: %page-n-columns%
		  page-number-format: ($page-number-format$ (normalize "lot"))
		  use: default-text-style
		  left-header:   ($left-header$ (normalize "lot"))
		  center-header: ($center-header$ (normalize "lot"))
		  right-header:  ($right-header$ (normalize "lot"))
		  left-footer:   ($left-footer$ (normalize "lot"))
		  center-footer: ($center-footer$ (normalize "lot"))
		  right-footer:  ($right-footer$ (normalize "lot"))
		  input-whitespace-treatment: 'collapse
		  (build-lot (current-node) (car gilist))
		  (loop (cdr gilist)))
		(loop (cdr gilist)))))

      (if (generate-toc-in-front)
	  (process-children)
	  (empty-sosofo)))))

(element (book title) (empty-sosofo))

(element part
  (let* ((partinfo  (select-elements (children (current-node)) 
				     (normalize "docinfo")))
	 (partintro (select-elements (children (current-node)) 
				     (normalize "partintro")))

	 (nl        (titlepage-info-elements 
		     (current-node) 
		     partinfo
		     (if %generate-partintro-on-titlepage%
			 partintro
			 (empty-node-list)))))
    (make sequence
      (if %generate-part-titlepage%
	  (make sequence
	    (part-titlepage nl 'recto)
	    (part-titlepage nl 'verso))
	  (empty-sosofo))

      (if (not (generate-toc-in-front))
	  (process-children)
	  (empty-sosofo))
      
      (if (and %generate-part-toc%
	       (not %generate-part-toc-on-titlepage%))
	  (make simple-page-sequence
	    page-n-columns: %page-n-columns%
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

      (if (and (not (node-list-empty? partintro))
	       (not %generate-partintro-on-titlepage%))
	  ($process-partintro$ partintro #t)
	  (empty-sosofo))
      
      (if (generate-toc-in-front)
	  (process-children)
	  (empty-sosofo)))))

(element (part title) (empty-sosofo))

(element partintro (empty-sosofo))

(element (partintro title) 
  (let* ((hlevel 1)
	 (hs (HSIZE (- 4 hlevel))))
    (make paragraph
      font-family-name: %title-font-family%
      font-weight:  (if (< hlevel 5) 'bold 'medium)
      font-posture: (if (< hlevel 5) 'upright 'italic)
      font-size: hs
      line-spacing: (* hs %line-spacing-factor%)
      space-before: (* hs %head-before-factor%)
      space-after:  (* hs %head-after-factor%)
      start-indent: 0pt
      first-line-start-indent: 0pt
      quadding: %section-title-quadding%
      keep-with-next?: #t
      heading-level: (if %generate-heading-level% (+ hlevel 1) 0)
      (element-title-sosofo (parent (current-node))))))

(define ($process-partintro$ partintro make-page-seq?)
  (if make-page-seq?
      (make simple-page-sequence
	page-n-columns: %page-n-columns%
	page-number-restart?: (or %page-number-restart% 
				  (book-start?) 
				  (first-chapter?))
	page-number-format: ($page-number-format$)
	use: default-text-style
	left-header:   ($left-header$)
	center-header: ($center-header$)
	right-header:  ($right-header$)
	left-footer:   ($left-footer$)
	center-footer: ($center-footer$)
	right-footer:  ($right-footer$)
	start-indent: %body-start-indent%
	input-whitespace-treatment: 'collapse
	quadding: %default-quadding%
	(make sequence
	  (process-node-list (children partintro))
	  (make-endnotes partintro)))
      (make sequence
	start-indent: %body-start-indent%
	(process-node-list (children partintro))
	(make-endnotes partintro))))

