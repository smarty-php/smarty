;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================= COMPONENTS =============================
;;
;; in docbook, components are containers at the chapter/appendix level

(define ($component$)
  (html-document 
   (with-mode head-title-mode 
     (literal (element-title-string (current-node))))
   ($component-body$)))

(define ($component-separator$) 
  (if (or (not nochunks) (node-list=? (current-node) (sgml-root-element)))
      (empty-sosofo)
      (make empty-element gi: "HR")))

(define ($component-body$)
  (let* ((epigraph (let loop ((nl (children (current-node))))
		     (if (node-list-empty? nl)
			 nl
			 (if (equal? (gi (node-list-first nl))
				     (normalize "epigraph"))
			     (node-list-first nl)
			     (if (or (equal? (gi (node-list-first nl))
					     (normalize "title"))
				     (equal? (gi (node-list-first nl))
					     (normalize "subtitle"))
				     (equal? (gi (node-list-first nl))
					     (normalize "titleabbrev"))
				     (equal? (gi (node-list-first nl))
					     (normalize "docinfo"))
				     (equal? (gi (node-list-first nl))
					     (normalize "chapterinfo"))
				     (equal? (gi (node-list-first nl))
					     (normalize "appendixinfo")))
				 (loop (node-list-rest nl))
				 (loop (empty-node-list)))))))
	 (notepigraph (let loop ((nl (children (current-node)))
				 (ch (empty-node-list)))
			(if (node-list-empty? nl)
			    ch
			    (if (node-list=? (node-list-first nl) epigraph)
				(loop (node-list-rest nl) ch)
				(loop (node-list-rest nl)
				      (node-list ch (node-list-first nl))))))))
    (make element gi: "DIV"
	  attributes: (list (list "CLASS" (gi)))
	  ($component-separator$)
	  ($component-title$)
	  (process-node-list epigraph)
	  (if ($generate-chapter-toc$)
	      ($chapter-toc$)
	      (empty-sosofo))
	  (process-node-list notepigraph))))

(define ($component-title$ #!optional (titlegi "H1") (subtitlegi "H2"))
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
      (make element gi: titlegi
	    (make sequence
	      (make element gi: "A"
		  attributes: (list (list "NAME" (element-id)))
		  (empty-sosofo))
	      (if (and %chapter-autolabel%
		       (or (equal? (gi) (normalize "chapter"))
			   (equal? (gi) (normalize "appendix"))))
		  (literal (gentext-element-name-space (gi))
			   (element-label (current-node))
			   (gentext-label-title-sep (gi)))
		  (empty-sosofo))
	      (if (node-list-empty? titles)
		  (element-title-sosofo) ;; get a default!
		  (with-mode title-mode
		    (process-node-list titles)))))
      (if (node-list-empty? subtitles) 
	  (empty-sosofo)
	  (with-mode subtitle-mode
	    (make element gi: subtitlegi
		  (process-node-list subtitles)))))))

(define ($chapter-toc$)
  ;; Called by the TITLE element so that it can come after the TITLE
  (build-toc (ancestor-member (current-node) (component-element-list))
	     (toc-depth 
	      (ancestor-member (current-node) (component-element-list)))
	     #t))

(element appendix ($component$))
(element (appendix title) (empty-sosofo))

(element chapter ($component$))
(element (chapter title) (empty-sosofo))

(element preface ($component$))
(element (preface title) (empty-sosofo))

;; Dedication is empty except in a special mode so that it can be
;; reordered (made to come before the TOCs)...see dbttlpg.dsl
;; Dedication is empty except in a special mode so that it can be
;; reordered (made to come before the TOCs)

(element dedication (empty-sosofo))

(mode dedication-page-mode
  (element dedication
    (html-document 
     (with-mode head-title-mode 
       (literal (element-title-string (current-node))))
     (make sequence
       ($component-separator$)
       ($component-title$)
       (process-children))))
  (element (dedication title) (empty-sosofo))
)

;; Articles are like components, except that if they may have much
;; more formal title pages (created with article-titlepage).
;;
(element article
  (let* ((info (node-list-filter-by-gi (children (current-node))
				       (list (normalize "artheader")
					     (normalize "articleinfo"))))
	 (ititle (select-elements (children info) (normalize "title")))
	 (title (if (node-list-empty? ititle)
		    (select-elements (children (current-node)) 
				     (normalize "title"))
		    (node-list-first ititle)))
	 (tsosofo (with-mode head-title-mode
		    (process-node-list title)))
	 (nl   (titlepage-info-elements (current-node) info)))
    (html-document
     tsosofo
     (make element gi: "DIV"
	   attributes: '(("CLASS" "ARTICLE"))
	   (if %generate-article-titlepage%
	       (make sequence
		 (article-titlepage nl 'recto)
		 (article-titlepage nl 'verso))
	       ($component-title$))

	   (if (not (generate-toc-in-front))
	       (process-children)
	       (empty-sosofo))
	  
	   (if %generate-article-toc%
	       (make sequence
		 (build-toc (current-node)
			    (toc-depth (current-node))))
	       (empty-sosofo))
	  
	   (let loop ((gilist ($generate-article-lot-list$)))
	     (if (null? gilist)
		 (empty-sosofo)
		 (if (not (node-list-empty?
			   (select-elements (descendants (current-node))
					    (car gilist))))
                          (make sequence
			    (build-lot (current-node) (car gilist))
			    (loop (cdr gilist)))
                          (loop (cdr gilist)))))

	   (if (generate-toc-in-front)
	       (process-children)
	       (empty-sosofo))))))

(element (article title) (empty-sosofo))

(element (article appendix) ($section$)) ;; this is a special case
