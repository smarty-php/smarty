;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; ============================= COMPONENTS =============================
;;
;; in docbook, components are containers at the chapter/appendix level

(define ($title-header-footer-element$)
  (let* ((firstchild (node-list-first (children (current-node))))
	 (metainfo   (if (node-list-empty? firstchild)
			 (empty-node-list)
			 (if (member (gi firstchild) (info-element-list))
			     firstchild
			     (empty-node-list))))
	 (metatitle  (select-elements (children metainfo) (normalize "title")))
	 (metatabb   (select-elements (children metainfo) (normalize "titleabbrev")))
	 (title      (select-elements (children (current-node)) 
				      (normalize "title")))
	 (titleabb   (select-elements (children (current-node)) 
				      (normalize "titleabbrev"))))
    (if (node-list-empty? metatabb)
	(if (node-list-empty? titleabb)
	    (if (node-list-empty? metatitle)
		title
		metatitle)
	    titleabb)
	metatabb)))
  
(define ($refentry-header-footer-element$)
  (let* ((refdescriptor (node-list-first 
			 (select-elements 
			  (descendants (current-node)) (normalize "refdescriptor"))))
	 (refname       (node-list-first 
			 (select-elements 
			  (descendants (current-node)) (normalize "refname"))))
         (refentrytitle (node-list-first 
			 (select-elements 
			  (descendants (current-node)) (normalize "refentrytitle")))))
    (if (node-list-empty? refentrytitle)
	(if (node-list-empty? refdescriptor)
	    refname
	    refdescriptor)
	refentrytitle)))

(define ($title-header-footer$)
  (let* ((title (if (equal? (gi) (normalize "refentry"))
		   ($refentry-header-footer-element$)
		   ($title-header-footer-element$))))
    (make sequence
      font-posture: 'italic
      (with-mode hf-mode 
	(process-node-list title)))))

(define ($page-number-header-footer$) 
  (let ((component (ancestor-member (current-node) 
				    (append (division-element-list)
					    (component-element-list)))))
    (make sequence
      font-posture: 'italic
      (literal 
       (gentext-page)
       (if %page-number-restart%
	   (cond
	    ((equal? (gi component) (normalize "appendix") ) 
	     (string-append
	      (element-label component #t)
	      (gentext-intra-label-sep "_pagenumber")))
	    ((equal? (gi component) (normalize "chapter"))
	     (string-append
	      (element-label component #t)
	      (gentext-intra-label-sep "_pagenumber")))
	    (else ""))
	   ""))
      (page-number-sosofo))))

(define (first-page-inner-header gi)
  (empty-sosofo))

(define (first-page-center-header gi)
  (empty-sosofo))

(define (first-page-outer-header gi)
  (empty-sosofo))

(define (page-inner-header gi)
  (empty-sosofo))

(define (page-center-header gi)
  (empty-sosofo))

(define (page-outer-header gi)
  (cond
   ((equal? (normalize gi) (normalize "dedication")) (empty-sosofo))
   ((equal? (normalize gi) (normalize "lot")) (empty-sosofo))
   ((equal? (normalize gi) (normalize "part")) (empty-sosofo))
   ((equal? (normalize gi) (normalize "toc")) (empty-sosofo))
   (else ($title-header-footer$))))

(define (first-page-inner-footer gi)
  (empty-sosofo))

(define (first-page-center-footer gi)
  (empty-sosofo))

(define (first-page-outer-footer gi)
  (cond
   ((equal? (normalize gi) (normalize "dedication")) (empty-sosofo))
   ((equal? (normalize gi) (normalize "part")) (empty-sosofo))
   (else ($page-number-header-footer$))))

(define (page-inner-footer gi)
  (empty-sosofo))

(define (page-center-footer gi)
  (empty-sosofo))

(define (page-outer-footer gi)
  ($page-number-header-footer$))

(define ($page-number-format$ #!optional (gi (gi)))
  (cond
   ((equal? (normalize gi) (normalize "toc")) "i")
   ((equal? (normalize gi) (normalize "lot")) "i")
   ((equal? (normalize gi) (normalize "preface")) "i")
   (else "1")))

(define ($left-header$ #!optional (gi (gi)))
  (if-first-page
   (if (equal? %writing-mode% 'left-to-right)
       (first-page-inner-header gi)
       (first-page-outer-header gi))
   (if %two-side%
       (if-front-page
	(if (equal? %writing-mode% 'left-to-right)
	    (page-inner-header gi)
	    (page-outer-header gi))
	(if (equal? %writing-mode% 'left-to-right)
	    (page-outer-header gi)
	    (page-inner-header gi)))
       (if (equal? %writing-mode% 'left-to-right)
	   (page-inner-header gi)
	   (page-outer-header gi)))))

(define ($center-header$ #!optional (gi (gi)))
  (if-first-page
   (first-page-center-header gi)
   (page-center-header gi)))

(define ($right-header$ #!optional (gi (gi)))
  (if-first-page
   (if (equal? %writing-mode% 'left-to-right)
       (first-page-outer-header gi)
       (first-page-inner-header gi))
   (if %two-side%
       (if-front-page
	(if (equal? %writing-mode% 'left-to-right)
	    (page-outer-header gi)
	    (page-inner-header gi))
	(if (equal? %writing-mode% 'left-to-right)
	    (page-inner-header gi)
	    (page-outer-header gi)))
       (if (equal? %writing-mode% 'left-to-right)
	   (page-outer-header gi)
	   (page-inner-header gi)))))

(define ($left-footer$ #!optional (gi (gi)))
  (if-first-page
   (if (equal? %writing-mode% 'left-to-right)
       (first-page-inner-footer gi)
       (first-page-outer-footer gi))
   (if %two-side%
       (if-front-page
	(if (equal? %writing-mode% 'left-to-right)
	    (page-inner-footer gi)
	    (page-outer-footer gi))
	(if (equal? %writing-mode% 'left-to-right)
	    (page-outer-footer gi)
	    (page-inner-footer gi)))
       (if (equal? %writing-mode% 'left-to-right)
	   (page-inner-footer gi)
	   (page-outer-footer gi)))))

(define ($center-footer$ #!optional (gi (gi)))
  (if-first-page
   (first-page-center-footer gi)
   (page-center-footer gi)))

(define ($right-footer$ #!optional (gi (gi)))
  (if-first-page
   (if (equal? %writing-mode% 'left-to-right)
       (first-page-outer-footer gi)
       (first-page-inner-footer gi))
   (if %two-side%
       (if-front-page
	(if (equal? %writing-mode% 'left-to-right)
	    (page-outer-footer gi)
	    (page-inner-footer gi))
	(if (equal? %writing-mode% 'left-to-right)
	    (page-inner-footer gi)
	    (page-outer-footer gi)))
       (if (equal? %writing-mode% 'left-to-right)
	   (page-outer-footer gi)
	   (page-inner-footer gi)))))

(define ($component$)
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
      ($component-title$)
      (process-children))
    (make-endnotes)))

(define ($component-title$)
  (let* ((info (cond
		((equal? (gi) (normalize "appendix"))
		 (select-elements (children (current-node)) (normalize "docinfo")))
		((equal? (gi) (normalize "article"))
		 (node-list-filter-by-gi (children (current-node))
					 (list (normalize "artheader")
					       (normalize "articleinfo"))))
		((equal? (gi) (normalize "bibliography"))
		 (select-elements (children (current-node)) (normalize "docinfo")))
		((equal? (gi) (normalize "chapter"))
		 (select-elements (children (current-node)) (normalize "docinfo")))
		((equal? (gi) (normalize "dedication")) 
		 (empty-node-list))
		((equal? (gi) (normalize "glossary"))
		 (select-elements (children (current-node)) (normalize "docinfo")))
		((equal? (gi) (normalize "index"))
		 (select-elements (children (current-node)) (normalize "docinfo")))
		((equal? (gi) (normalize "preface"))
		 (select-elements (children (current-node)) (normalize "docinfo")))
		((equal? (gi) (normalize "reference"))
		 (select-elements (children (current-node)) (normalize "docinfo")))
		((equal? (gi) (normalize "setindex"))
		 (select-elements (children (current-node)) (normalize "docinfo")))
		(else
		 (empty-node-list))))
	 (exp-children (if (node-list-empty? info)
			   (empty-node-list)
			   (expand-children (children info) 
					    (list (normalize "bookbiblio") 
						  (normalize "bibliomisc")
						  (normalize "biblioset")))))
	 (parent-titles (select-elements (children (current-node)) (normalize "title")))
	 (info-titles   (select-elements exp-children (normalize "title")))
	 (titles        (if (node-list-empty? parent-titles)
			    info-titles
			    parent-titles))
	 (subtitles     (select-elements exp-children (normalize "subtitle"))))
    (make sequence
      (make paragraph
	font-family-name: %title-font-family%
	font-weight: 'bold
	font-size: (HSIZE 4)
	line-spacing: (* (HSIZE 4) %line-spacing-factor%)
	space-before: (* (HSIZE 4) %head-before-factor%)
	start-indent: 0pt
	first-line-start-indent: 0pt
	quadding: %component-title-quadding%
	heading-level: (if %generate-heading-level% 1 0)
	keep-with-next?: #t

	(if (string=? (element-label) "")
	    (empty-sosofo)
	    (literal (gentext-element-name-space (current-node))
		     (element-label)
		     (gentext-label-title-sep (gi))))

	(if (node-list-empty? titles)
	    (element-title-sosofo) ;; get a default!
	    (with-mode component-title-mode
	      (make sequence
		(process-node-list titles)))))

      (make paragraph
	font-family-name: %title-font-family%
	font-weight: 'bold
	font-posture: 'italic
	font-size: (HSIZE 3)
	line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	space-before: (* 0.5 (* (HSIZE 3) %head-before-factor%))
	space-after: (* (HSIZE 4) %head-after-factor%)
	start-indent: 0pt
	first-line-start-indent: 0pt
	quadding: %component-subtitle-quadding%
	keep-with-next?: #t

	(with-mode component-title-mode
	  (make sequence
	    (process-node-list subtitles)))))))

(mode component-title-mode
  (element title
    (process-children))

  (element subtitle
    (process-children))
)

;; this is how we prevent the title in the header from acquiring the
;;   display treatment that it receives in the body of the document
;;
(mode hf-mode
  (element title
    (let* ((component (ancestor-member (current-node) 
				       (component-element-list)))
	   (chaporapp (or (equal? (gi component) (normalize "chapter"))
			  (equal? (gi component) (normalize "appendix")))))
      (if %chap-app-running-heads%
	  (make sequence
	    (if (and chaporapp
		     %chapter-autolabel%
		     (or %chap-app-running-head-autolabel%
			 (attribute-string (normalize "label") component)))
		(literal (gentext-element-name-space component)
			 (element-label component)
			 (gentext-label-title-sep (gi component)))
		(empty-sosofo))
	    (process-children-trim))
	  (empty-sosofo))))

  (element titleabbrev
    (if %chap-app-running-heads%
	(make sequence
	  (if (or (have-ancestor? (normalize "chapter"))
		  (have-ancestor? (normalize "appendix")))
	      (literal (gentext-element-name-space (parent))
		       (element-label (parent))
		       (gentext-label-title-sep (gi (parent))))
	      (empty-sosofo))
	  (process-children-trim))
	(empty-sosofo)))

  (element refentrytitle
    (if %chap-app-running-heads%
	(process-children-trim)
	(empty-sosofo)))

  (element refdescriptor
    (if %chap-app-running-heads%
	(process-children-trim)
	(empty-sosofo)))

  (element refname
    (if %chap-app-running-heads%
	(process-children-trim)
	(empty-sosofo)))

  ;; Graphics aren't allowed in headers and footers...
  (element graphic
    (empty-sosofo))

  (element inlinegraphic
    (empty-sosofo))
)

(element appendix ($component$))
(element (article appendix) ($section$)) ;; this is a special case
(element (appendix title) (empty-sosofo))

(element chapter ($component$))
(element (chapter title) (empty-sosofo))

(element preface ($component$))
(element (preface title) (empty-sosofo))

;; Dedication is empty except in a special mode so that it can be
;; reordered (made to come before the TOCs)
(element dedication (empty-sosofo))
(mode dedication-page-mode
  (element dedication ($component$))
  (element (dedication title) (empty-sosofo))
)

;; Articles are like components, except that if they may have much
;; more formal title pages (created with article-titlepage).
;;
(element article
  (let* ((info (node-list-filter-by-gi (children (current-node))
				       (list (normalize "artheader")
					     (normalize "articleinfo"))))
	 (nl   (titlepage-info-elements (current-node) info))
	 (article-titlepage (if %generate-article-titlepage-on-separate-page%
				(make sequence
				  (if (article-titlepage-content? nl 'recto)
				      (make simple-page-sequence
					page-n-columns: %page-n-columns%
					use: default-text-style
					quadding: %default-quadding%
					(article-titlepage nl 'recto))
				      (empty-sosofo))
				  (if (article-titlepage-content? nl 'verso)
				      (make simple-page-sequence
					page-n-columns: %page-n-columns%
					use: default-text-style
					quadding: %default-quadding%
					(article-titlepage nl 'verso))
				      (empty-sosofo)))
				(make sequence
				  (article-titlepage nl 'recto)
				  (article-titlepage nl 'verso)))))
    (make sequence
      (if (and %generate-article-titlepage% 
	       %generate-article-titlepage-on-separate-page%)
	  article-titlepage
	  (empty-sosofo))

      (if (and %generate-article-toc% 
	       (not %generate-article-toc-on-titlepage%)
	       %generate-article-titlepage-on-separate-page%
	       (generate-toc-in-front))
	  (make simple-page-sequence
	    page-n-columns: %page-n-columns%
	    page-number-restart?: %article-page-number-restart%
	    page-number-format: ($page-number-format$ (normalize "toc"))
	    left-header:   ($left-header$ (normalize "toc"))
	    center-header: ($center-header$ (normalize "toc"))
	    right-header:  ($right-header$ (normalize "toc"))
	    left-footer:   ($left-footer$ (normalize "toc"))
	    center-footer: ($center-footer$ (normalize "toc"))
	    right-footer:  ($right-footer$ (normalize "toc"))
	    input-whitespace-treatment: 'collapse
	    use: default-text-style
	    quadding: %default-quadding%
	    (build-toc (current-node)
		       (toc-depth (current-node))))
	  (empty-sosofo))
	  
      (make simple-page-sequence
	page-n-columns: %page-n-columns%
	page-number-restart?: (or %article-page-number-restart% 
				  (book-start?))
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

	(if (and %generate-article-titlepage% 
		 (not %generate-article-titlepage-on-separate-page%))
	    article-titlepage
	    (empty-sosofo))

	(if (and %generate-article-toc% 
		 (generate-toc-in-front)
		 (not %generate-article-toc-on-titlepage%)
		 (not %generate-article-titlepage-on-separate-page%))
	    (make display-group
	      space-after: (* (HSIZE 3) %head-after-factor%)
	      (build-toc (current-node)
			 (toc-depth (current-node))))
	    (empty-sosofo))

	(process-children)

	(make-endnotes)

	(if (and %generate-article-toc% 
		 (not (generate-toc-in-front))
		 (not %generate-article-toc-on-titlepage%)
		 (not %generate-article-titlepage-on-separate-page%))
	    (make display-group
	      space-after: (* (HSIZE 3) %head-after-factor%)
	      (build-toc (current-node)
			 (toc-depth (current-node))))
	    (empty-sosofo)))

      (if (and %generate-article-toc% 
	       (not %generate-article-toc-on-titlepage%)
	       %generate-article-titlepage-on-separate-page%
	       (not (generate-toc-in-front)))
	  (make simple-page-sequence
	    page-n-columns: %page-n-columns%
	    use: default-text-style
	    quadding: %default-quadding%
	    (build-toc (current-node)
		       (toc-depth (current-node))))
	  (empty-sosofo)))))

(element (article title) (empty-sosofo))

