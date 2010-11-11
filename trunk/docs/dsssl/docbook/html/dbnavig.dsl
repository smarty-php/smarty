;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; The header of a chunk has this form:
;;
;; +-----------------------------------------+
;; |               nav-banner                |
;; +------------+---------------+------------|
;; |  prevlink  |  nav-context  |  nextlink  |
;; +-----------------------------------------+

(define (nav-banner? elemnode)
  ;; This node has a banner if:
  ;; 1. There's an inherited dbhtml PI value for "banner-text" and that
  ;;    value is not the empty string, or
  ;; 2. The element is not the root element
  (let ((banner (inherited-dbhtml-value elemnode "banner-text")))
    (or (and banner (not (string=? banner "")))
	(not (node-list=? elemnode (sgml-root-element))))))

(define (nav-banner elemnode)
  (let* ((rootelem       (sgml-root-element))
	 (info           (info-element rootelem))
	 (subtitle-child (select-elements (children rootelem)
					  (normalize "subtitle")))
	 (subtitle-info  (select-elements (children info)
					  (normalize "subtitle")))
	 (subtitle       (if (node-list-empty? subtitle-info)
			     subtitle-child
			     subtitle-info))
	 (banner-text    (inherited-dbhtml-value elemnode "banner-text"))
	 (banner-href    (inherited-dbhtml-value elemnode "banner-href"))
	 (banner         (if (and banner-text (not (string=? banner-text "")))
			     (literal banner-text)
			     (make sequence
			       (element-title-sosofo rootelem)
			       (if (node-list-empty? subtitle)
				   (empty-sosofo)
				   (make sequence
				     (literal ": ")
				     (with-mode subtitle-mode
				       (process-node-list subtitle))))))))
    (make sequence
      (if banner-href
	  (make element gi: "A"
		attributes: (list (list "HREF" banner-href))
		banner)
	  banner))))

(define (nav-context? elemnode)
  ;; Print a context header if
  ;; 1. There's an inherited dbhtml PI value for "context-text" and that
  ;;    value is not the empty string, or
  ;; 2. The chunk is a top level section and the parent component 
  ;;    isn't the same as the root element (which appears in the nav-banner).
  ;;
  (let* ((context-text (inherited-dbhtml-value elemnode "context-text"))
	 (rootelem     (sgml-root-element))
	 (component    (ancestor-member elemnode
					(append (book-element-list)
						(division-element-list)
						(component-element-list))))
	 (gencontext   (and (or (equal? (gi elemnode) (normalize "sect1"))
				(equal? (gi elemnode) (normalize "section")))
			    (not (node-list=? component rootelem)))))
    (or gencontext
	(and context-text (not (string=? context-text ""))))))

(define (nav-context elemnode)
  ;; Print the context string for elemnode.  If there's an inherited
  ;; dbhtml value for 'context-text', use that.  Otherwise, use the
  ;; title of the parent component...
  (let* ((context-href  (inherited-dbhtml-value elemnode "context-href")))
    (if (nav-context? elemnode)
	(if context-href
	    (make element gi: "A"
		  attributes: (list (list "HREF" context-href))
		  (nav-context-sosofo elemnode))
	    (nav-context-sosofo elemnode))
	(empty-sosofo))))

(define (nav-context-sosofo elemnode)
  (let* ((component     (ancestor-member elemnode
					 (append (book-element-list)
						 (division-element-list)
						 (component-element-list))))
	 (context-text  (inherited-dbhtml-value elemnode "context-text")))
    (if (and context-text (not (string=? context-text "")))
	(literal context-text)
	(if (equal? (element-label component) "")
	    (make sequence
	      (element-title-sosofo component))
	    (make sequence
	      ;; Special case.  This is a bit of a hack.
	      ;; I need to revisit this aspect of 
	      ;; appendixes. 
	      (if (and (equal? (gi component) (normalize "appendix"))
		       (or (equal? (gi elemnode) (normalize "sect1"))
			   (equal? (gi elemnode) (normalize "section")))
		       (equal? (gi (parent component)) (normalize "article")))
		  (empty-sosofo)
		  (literal (gentext-element-name-space (gi component))))
	      (element-label-sosofo component)
	      (literal (gentext-label-title-sep (gi component)))
	      (element-title-sosofo component))))))

;; The footer of a chunk has this form:
;;
;; +----------------------------------------+
;; |  prevlink  |   nav-home   |  nextlink  |
;; +------------+--------------+------------|
;; |  p. title  |    nav-up    |  n. title  |
;; +-----------------------------------------+

(define (nav-home? elemnode)
  (not (node-list=? elemnode (sgml-root-element))))

(define (nav-home elemnode)
  (sgml-root-element))

(define (nav-home-link elemnode)
  (let ((home      (nav-home elemnode))
	(home-text (inherited-dbhtml-value elemnode "home-text")))
    (if (node-list=? elemnode home)
	(make entity-ref name: "nbsp")
	(make element gi: "A"
	      attributes: (list
			   (list "HREF" 
				 (href-to home))
			   (list "ACCESSKEY" "H"))
	      (if home-text
		  (literal home-text)
		  (gentext-nav-home home))))))

;; nav-up is displayed in the bottom center of the footer-navigation
;; table.  The definition below will show "Up" for nested components
;; (the component wrapping a section, the division wrapping a component
;; etc.).  It can be abused for other things, such as an index...
;;
(define (nav-up? elemnode)
  (let ((up      (parent elemnode))
	(up-text (inherited-dbhtml-value elemnode "up-text")))
    (if (and up-text (not (string=? up-text "")))
	#t
	(if (or (node-list-empty? up)
		(node-list=? up (sgml-root-element))
		(equal? (gi up) (normalize "bookinfo"))
		(equal? (gi up) (normalize "docinfo"))
		(equal? (gi up) (normalize "setinfo")))
	    #f
	    #t))))

(define (nav-up elemnode)
  (let* ((up      (parent elemnode))
	 (up-href (inherited-dbhtml-value elemnode "up-href"))
	 (uplink? (not (or (node-list-empty? up)
			   (node-list=? up (sgml-root-element)))))
	 (href    (if up-href
		      up-href
		      (if uplink?
			  (href-to up)
			  #f))))
    (if href
	(make element gi: "A"
	      attributes: (list
			   (list "HREF" href)
			   (list "ACCESSKEY" "U"))
	      (nav-up-sosofo elemnode))
	(nav-up-sosofo elemnode))))

(define (nav-up-sosofo elemnode)
  (let* ((up      (parent elemnode))
	 (up-text (inherited-dbhtml-value elemnode "up-text")))
    (if (and up-text (not (string=? up-text "")))
	(literal up-text)
	(if (or (node-list-empty? up)
		(node-list=? up (sgml-root-element)))
	    (make entity-ref name: "nbsp")
	    (gentext-nav-up up)))))

(define (nav-footer elemnode)
  (empty-sosofo))

;; ======================================================================

(define (header-navigation nd #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element nd)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element nd)
		    (list-ref navlist 1)))
	 (prevm (if (null? navlist)
		    (prev-major-component-chunk-element nd)
		    (list-ref navlist 2)))
	 (nextm (if (null? navlist)
		    (next-major-component-chunk-element nd)
		    (list-ref navlist 3)))
	 (rnavlist (list prev next prevm nextm)))
    (make sequence
      ($html-body-start$)
      (if %header-navigation%
	  (cond 
	   ((equal? (gi nd) (normalize "set"))
	    (set-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "book"))
	    (book-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "part"))
	    (part-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "preface"))
	    (preface-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "chapter"))
	    (chapter-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "article"))
	    (article-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "appendix"))
	    (appendix-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "reference"))
	    (reference-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "refentry"))
	    (refentry-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "glossary"))
	    (glossary-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "bibliography"))
	    (bibliography-header-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "index"))
	    (index-header-navigation nd rnavlist))
	   ;; LegalNotice only happens when %generate-legalnotice-link% is #t
	   ((equal? (gi nd) (normalize "legalnotice"))
	    (default-header-navigation nd
	      (empty-node-list) (empty-node-list)
	      (empty-node-list) (empty-node-list)))
	   ((member (gi nd) (section-element-list))
	    (section-header-navigation nd rnavlist))
	   (else (default-header-navigation nd prev next prevm nextm)))
	  (empty-sosofo))
      ($user-header-navigation$ prev next prevm nextm)
      ($html-body-content-start$))))

(define (footer-navigation nd #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element nd)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element nd)
		    (list-ref navlist 1)))
	 (prevm (if (null? navlist)
		    (prev-major-component-chunk-element nd)
		    (list-ref navlist 2)))
	 (nextm (if (null? navlist)
		    (next-major-component-chunk-element nd)
		    (list-ref navlist 3)))
	 (rnavlist (list prev next prevm nextm)))
    (make sequence
      (make-endnotes)
      ($html-body-content-end$)
      ($user-footer-navigation$ prev next prevm nextm)
      (if %footer-navigation%
	  (cond 
	   ((equal? (gi nd) (normalize "set"))          
	    (set-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "book"))
	    (book-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "part"))
	    (part-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "preface"))
	    (preface-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "chapter"))
	    (chapter-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "article"))
	    (article-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "appendix"))
	    (appendix-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "reference"))
	    (reference-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "refentry"))
	    (refentry-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "glossary"))
	    (glossary-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "bibliography"))
	    (bibliography-footer-navigation nd rnavlist))
	   ((equal? (gi nd) (normalize "index"))
	    (index-footer-navigation nd rnavlist))
	   ;; LegalNotice only happens when %generate-legalnotice-link% is #t
	   ((equal? (gi nd) (normalize "legalnotice"))  
	    (default-footer-navigation nd
	      (empty-node-list) (empty-node-list)
	      (empty-node-list) (empty-node-list)))
	   ((member (gi nd) (section-element-list))
	    (section-footer-navigation nd rnavlist))
	   (else (default-footer-navigation nd prev next prevm nextm)))
	  (empty-sosofo))
      (nav-footer nd)
      ($html-body-end$))))

(define (set-header-navigation elemnode #!optional (navlist '()))
  (empty-sosofo))

(define (book-header-navigation elemnode #!optional (navlist '()))
  (empty-sosofo))

(define (part-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (preface-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (chapter-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (appendix-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (article-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (if (node-list=? elemnode (sgml-root-element))
	(empty-sosofo)
	(default-header-navigation elemnode prev next prevsib nextsib))))

(define (glossary-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (bibliography-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (index-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (reference-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (refentry-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (section-header-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-header-navigation elemnode prev next prevsib nextsib)))

(define (set-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (book-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (part-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (preface-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (chapter-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (appendix-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (article-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (glossary-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (bibliography-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (index-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (reference-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (refentry-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

(define (section-footer-navigation elemnode #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element elemnode)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element elemnode)
		    (list-ref navlist 1)))
	 (prevsib (if (null? navlist)
		    (prev-major-component-chunk-element elemnode)
		    (list-ref navlist 2)))
	 (nextsib (if (null? navlist)
		    (next-major-component-chunk-element elemnode)
		    (list-ref navlist 3))))
    (default-footer-navigation elemnode prev next prevsib nextsib)))

;; ----------------------------------------------------------------------

(define (default-header-nav-tbl-ff elemnode prev next prevsib nextsib)
  (let* ((r1? (nav-banner? elemnode))
	 (r1-sosofo (make element gi: "TR"
			  (make element gi: "TH"
				attributes: (list
					     (list "COLSPAN" "5")
					     (list "ALIGN" "center")
					     (list "VALIGN" "bottom"))
				(nav-banner elemnode))))
	 (r2? (or (not (node-list-empty? prev))
		  (not (node-list-empty? next))
		  (not (node-list-empty? prevsib))
		  (not (node-list-empty? nextsib))
		  (nav-context? elemnode)))
	 (r2-sosofo (make element gi: "TR"
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "left")
					     (list "VALIGN" "top"))
				(if (node-list-empty? prev)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF"
							     (href-to
							      prev))
						       (list "ACCESSKEY"
							     "P"))
					  (gentext-nav-prev prev))))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "left")
					     (list "VALIGN" "top"))
				(if (node-list-empty? prevsib)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF"
							     (href-to
							      prevsib)))
					  (gentext-nav-prev-sibling prevsib))))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "60%")
					     (list "ALIGN" "center")
					     (list "VALIGN" "bottom"))
				(nav-context elemnode))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "right")
					     (list "VALIGN" "top"))
				(if (node-list-empty? nextsib)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to
							      nextsib)))
					  (gentext-nav-next-sibling nextsib))))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "right")
					     (list "VALIGN" "top"))
				(if (node-list-empty? next)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to
							      next))
						       (list "ACCESSKEY"
							     "N"))
					  (gentext-nav-next next)))))))
    (if (or r1? r2?)
	(make element gi: "DIV"
	      attributes: '(("CLASS" "NAVHEADER"))
	  (make element gi: "TABLE"
		attributes: (list
			     (list "SUMMARY" "Header navigation table")
			     (list "WIDTH" %gentext-nav-tblwidth%)
			     (list "BORDER" "0")
			     (list "CELLPADDING" "0")
			     (list "CELLSPACING" "0"))
		(if r1? r1-sosofo (empty-sosofo))
		(if r2? r2-sosofo (empty-sosofo)))
	  (make empty-element gi: "HR"
		attributes: (list
			     (list "ALIGN" "LEFT")
			     (list "WIDTH" %gentext-nav-tblwidth%))))
	(empty-sosofo))))

(define (default-header-nav-tbl-noff elemnode prev next prevsib nextsib)
  (let* ((r1? (nav-banner? elemnode))
	 (r1-sosofo (make element gi: "TR"
			  (make element gi: "TH"
				attributes: (list
					     (list "COLSPAN" "3")
					     (list "ALIGN" "center"))
				(nav-banner elemnode))))
	 (r2? (or (not (node-list-empty? prev))
		  (not (node-list-empty? next))
		  (nav-context? elemnode)))
	 (r2-sosofo (make element gi: "TR"
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "left")
					     (list "VALIGN" "bottom"))
				(if (node-list-empty? prev)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to 
							      prev))
						       (list "ACCESSKEY"
							     "P"))
					  (gentext-nav-prev prev))))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "80%")
					     (list "ALIGN" "center")
					     (list "VALIGN" "bottom"))
				(nav-context elemnode))
			  (make element gi: "TD"
				attributes: (list
					     (list "WIDTH" "10%")
					     (list "ALIGN" "right")
					     (list "VALIGN" "bottom"))
				(if (node-list-empty? next)
				    (make entity-ref name: "nbsp")
				    (make element gi: "A"
					  attributes: (list
						       (list "HREF" 
							     (href-to
							      next))
						       (list "ACCESSKEY"
							     "N"))
					  (gentext-nav-next next)))))))
    (if (or r1? r2?)
	(make element gi: "DIV"
	      attributes: '(("CLASS" "NAVHEADER"))
	  (make element gi: "TABLE"
		attributes: (list
			     (list "SUMMARY" "Header navigation table")
			     (list "WIDTH" %gentext-nav-tblwidth%)
			     (list "BORDER" "0")
			     (list "CELLPADDING" "0")
			     (list "CELLSPACING" "0"))
		(if r1? r1-sosofo (empty-sosofo))
		(if r2? r2-sosofo (empty-sosofo)))
	  (make empty-element gi: "HR"
		attributes: (list
			     (list "ALIGN" "LEFT")
			     (list "WIDTH" %gentext-nav-tblwidth%))))
	(empty-sosofo))))

(define (default-header-nav-notbl-ff elemnode prev next prevsib nextsib)
  (make element gi: "DIV"
	attributes: '(("CLASS" "NAVHEADER"))
	(if (nav-banner? elemnode)
	    (make element gi: "H1"
		  (nav-banner elemnode))
	    (empty-sosofo))

	(if (and (node-list-empty? prev)
		 (node-list-empty? prevsib)
		 (node-list-empty? nextsib)
		 (node-list-empty? next))
	    (empty-sosofo)
	    (make element gi: "P"
		  (if (node-list-empty? next)
		      (empty-sosofo)
		      (make sequence 
			(make element gi: "A"
			      attributes: (list
					   (list "HREF" (href-to next))
					   (list "ACCESSKEY" "N"))
			      (gentext-nav-next next))))

		  (if (node-list-empty? prev)
		      (empty-sosofo)
		      (make sequence
			(if (node-list-empty? next)
			    (empty-sosofo)
			    (literal ", "))
			(make element gi: "A"
			      attributes: (list
					   (list "HREF" (href-to prev))
					   (list "ACCESSKEY" "P"))
			      (gentext-nav-prev prev))))
		  
		  (if (node-list-empty? nextsib)
		      (empty-sosofo)
		      (make sequence 
			(if (and (node-list-empty? next)
				 (node-list-empty? prev))
			    (empty-sosofo)
			    (literal ", "))
			(make element gi: "A"
			      attributes: (list
					   (list "HREF" (href-to nextsib)))
			      (gentext-nav-next-sibling nextsib))))

		  (if (node-list-empty? prevsib)
		      (empty-sosofo)
		      (make sequence 
			(if (and (node-list-empty? next)
				 (node-list-empty? prev)
				 (node-list-empty? nextsib))
			    (empty-sosofo)
			    (literal ", "))
			(make element gi: "A"
			      attributes: (list
					   (list "HREF" (href-to prevsib)))
			      (gentext-nav-prev-sibling prevsib))))))
	
	(if (nav-context? elemnode)
	    (make element gi: "H2"
		  (nav-context elemnode))
	    (empty-sosofo))
	
	(make empty-element gi: "HR")))

(define (default-header-nav-notbl-noff elemnode prev next prevsib nextsib)
  (default-header-nav-notbl-ff elemnode prev next 
    (empty-node-list) (empty-node-list)))

(define (default-header-navigation elemnode prev next prevsib nextsib)
  (if %gentext-nav-use-tables%
      (if %gentext-nav-use-ff% 
	  (default-header-nav-tbl-ff elemnode prev next prevsib nextsib)
	  (default-header-nav-tbl-noff elemnode prev next prevsib nextsib))
      (if %gentext-nav-use-ff% 
	  (default-header-nav-notbl-ff elemnode prev next prevsib nextsib)
	  (default-header-nav-notbl-noff elemnode prev next prevsib nextsib))))

(define (default-footer-navigation elemnode prev next prevsib nextsib)
  (if %gentext-nav-use-tables%
      (default-footer-nav-tbl elemnode prev next prevsib nextsib)
      (default-footer-nav-notbl elemnode prev next prevsib nextsib)))

(define (default-footer-nav-tbl elemnode prev next prevsib nextsib)
  (let ((r1? (or (not (node-list-empty? prev))
		 (not (node-list-empty? next))
		 (nav-home? elemnode)))
	(r2? (or (not (node-list-empty? prev))
		 (not (node-list-empty? next))
		 (nav-up? elemnode)))

	(r1-sosofo (make element gi: "TR"
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "33%")
					    (list "ALIGN" "left")
					    (list "VALIGN" "top"))
			       (if (node-list-empty? prev)
				   (make entity-ref name: "nbsp")
				   (make element gi: "A"
					 attributes: (list
						      (list "HREF" (href-to
								    prev))
						      (list "ACCESSKEY"
							    "P"))
					 (gentext-nav-prev prev))))
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "34%")
					    (list "ALIGN" "center")
					    (list "VALIGN" "top"))
			       (nav-home-link elemnode))
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "33%")
					    (list "ALIGN" "right")
					    (list "VALIGN" "top"))
			       (if (node-list-empty? next)
				   (make entity-ref name: "nbsp")
				   (make element gi: "A"
					 attributes: (list
						      (list "HREF" (href-to
								    next))
						      (list "ACCESSKEY"
							    "N"))
					 (gentext-nav-next next))))))
	(r2-sosofo (make element gi: "TR"
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "33%")
					    (list "ALIGN" "left")
					    (list "VALIGN" "top"))
			       (if (node-list-empty? prev)
				   (make entity-ref name: "nbsp")
				   (element-title-sosofo prev)))
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "34%")
					    (list "ALIGN" "center")
					    (list "VALIGN" "top"))
			       (if (nav-up? elemnode)
				   (nav-up elemnode)
				   (make entity-ref name: "nbsp")))
			 (make element gi: "TD"
			       attributes: (list
					    (list "WIDTH" "33%")
					    (list "ALIGN" "right")
					    (list "VALIGN" "top"))
			       (if (node-list-empty? next)
				   (make entity-ref name: "nbsp")
				   (element-title-sosofo next))))))
    (if (or r1? r2?)
	(make element gi: "DIV"
	      attributes: '(("CLASS" "NAVFOOTER"))
	  (make empty-element gi: "HR"
		attributes: (list
			     (list "ALIGN" "LEFT") 
			     (list "WIDTH" %gentext-nav-tblwidth%)))
	  (make element gi: "TABLE"
		attributes: (list
			     (list "SUMMARY" "Footer navigation table")
			     (list "WIDTH" %gentext-nav-tblwidth%)
			     (list "BORDER" "0")
			     (list "CELLPADDING" "0")
			     (list "CELLSPACING" "0"))
		(if r1? r1-sosofo (empty-sosofo))
		(if r2? r2-sosofo (empty-sosofo))))
	(empty-sosofo))))

(define (default-footer-nav-notbl elemnode prev next prevsib nextsib)
  (make element gi: "DIV"
	attributes: '(("CLASS" "NAVFOOTER"))
	(make empty-element gi: "HR")
    
	(if (nav-home? elemnode)
	    (nav-home-link elemnode)
	    (empty-sosofo))

	(if (nav-up? elemnode)
	    (make sequence
	      (if (nav-home? elemnode)
		  (literal ", ")
		  (empty-sosofo))
	      (nav-up elemnode))
	    (empty-sosofo))
    
	(if (or (nav-home? elemnode) (nav-up? elemnode))
	    (make empty-element gi: "BR")
	    (empty-sosofo))

	(if (node-list-empty? prev)
	    (empty-sosofo)
	    (make sequence
	      (make element gi: "A"
		    attributes: (list
				 (list "HREF" (href-to prev))
				 (list "ACCESSKEY" "P"))
		    (gentext-nav-prev prev))
	      (literal ": " (element-title-string prev))
	      (make empty-element gi: "BR")))
	
	(if (node-list-empty? next)
	    (empty-sosofo)
	    (make sequence
	      (make element gi: "A"
		    attributes: (list
				 (list "HREF" (href-to next))
				 (list "ACCESSKEY" "N"))
		    (gentext-nav-next next))
	      (literal ": " (element-title-string next))
	      (make empty-element gi: "BR")))))

(define ($user-header-navigation$ #!optional 
				  (prev (empty-node-list))
				  (next (empty-node-list))
				  (prevm (empty-node-list))
				  (nextm (empty-node-list)))
  (empty-sosofo))

(define ($user-footer-navigation$ #!optional 
				  (prev (empty-node-list))
				  (next (empty-node-list))
				  (prevm (empty-node-list))
				  (nextm (empty-node-list)))
  (empty-sosofo))

;; EOF dbnavig.dsl;
