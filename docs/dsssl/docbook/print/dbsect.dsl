;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================== SECTIONS ==============================

(define (SECTLEVEL #!optional (sect (current-node)))
  (section-level-by-node #f sect))

;; BRIDGEHEAD isn't a proper section, but appears to be a section title
(element bridgehead
  (let* ((renderas (attribute-string "renderas"))
	 ;; the apparent section level
	 (hlevel
	  ;; if not real section level, then get the apparent level
	  ;; from "renderas"
	  (if renderas
	      (section-level-by-gi #f (normalize renderas))
	      ;; else use the real level
	      (SECTLEVEL)))
	 (hs (HSIZE (- 4 hlevel))))	
    (make paragraph
      font-family-name: %title-font-family%
      font-weight:  (if (< hlevel 5) 'bold 'medium)
      font-posture: (if (< hlevel 5) 'upright 'italic)
      font-size: hs
      line-spacing: (* hs %line-spacing-factor%)
      space-before: (* hs %head-before-factor%)
      space-after: (* hs %head-after-factor%)
      start-indent: (if (< hlevel 3)
			0pt
			%body-start-indent%)
      first-line-start-indent: 0pt
      quadding: %section-title-quadding%
      keep-with-next?: #t
      (process-children))))

(define ($section$)
  (if (node-list=? (current-node) (sgml-root-element))
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
	  ($section-title$)
	  (process-children)))
      (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	start-indent: %body-start-indent%
	(make sequence
	  ($section-title$)
	  (process-children)))))

(define ($section-title$)
  (let* ((sect (current-node))
	 (info (info-element))
	 (exp-children (if (node-list-empty? info)
			   (empty-node-list)
			   (expand-children (children info) 
					    (list (normalize "bookbiblio") 
						  (normalize "bibliomisc")
						  (normalize "biblioset")))))
	 (parent-titles (select-elements (children sect) (normalize "title")))
	 (info-titles   (select-elements exp-children (normalize "title")))
	 (titles        (if (node-list-empty? parent-titles)
			    info-titles
			    parent-titles))
	 (subtitles     (select-elements exp-children (normalize "subtitle")))
	 (renderas (inherited-attribute-string (normalize "renderas") sect))
	 ;; the apparent section level
	 (hlevel
	  ;; if not real section level, then get the apparent level
	  ;; from "renderas"
	  (if renderas
	      (section-level-by-gi #f (normalize renderas))
	      ;; else use the real level
	      (SECTLEVEL)))
	 (hs (HSIZE (- 4 hlevel))))
    (make sequence
      (make paragraph
	font-family-name: %title-font-family%
	font-weight:  (if (< hlevel 5) 'bold 'medium)
	font-posture: (if (< hlevel 5) 'upright 'italic)
	font-size: hs
	line-spacing: (* hs %line-spacing-factor%)
	space-before: (* hs %head-before-factor%)
	space-after: (if (node-list-empty? subtitles)
			 (* hs %head-after-factor%)
			 0pt)
	start-indent: (if (or (>= hlevel 3)
			      (member (gi) (list (normalize "refsynopsisdiv") 
						 (normalize "refsect1") 
						 (normalize "refsect2") 
						 (normalize "refsect3"))))
			  %body-start-indent%
			  0pt)
	first-line-start-indent: 0pt
	quadding: %section-title-quadding%
	keep-with-next?: #t
	heading-level: (if %generate-heading-level% hlevel 0)
	;; SimpleSects are never AUTO numbered...they aren't hierarchical
	(if (string=? (element-label (current-node)) "")
	    (empty-sosofo)
	    (literal (element-label (current-node)) 
		     (gentext-label-title-sep (gi sect))))
	(element-title-sosofo (current-node)))
      (with-mode section-title-mode
	(process-node-list subtitles))
      ($proc-section-info$ info))))

(mode section-title-mode
  (element subtitle
    (let* ((sect (parent (parent (current-node)))) ;; parent=>sect*info
	   (renderas (inherited-attribute-string "renderas" sect))
	   ;; the apparent section level
	   (hlevel
	    ;; if not real section level, then get the apparent level
	    ;; from "renderas"
	    (if renderas
		(section-level-by-gi #f (normalize renderas))
		;; else use the real level
		(SECTLEVEL)))
	   (hs (HSIZE (- 3 hlevel))))       ;; one smaller than the title...
      (make paragraph
	font-family-name: %title-font-family%
	font-weight:  (if (< hlevel 5) 'bold 'medium)
	font-posture: (if (< hlevel 5) 'upright 'italic)
	font-size: hs
	line-spacing: (* hs %line-spacing-factor%)
	space-before: 0pt
	space-after: (* hs %head-after-factor%)
	start-indent:
	(if (< hlevel 3)
	    0pt
	    %body-start-indent%)
	first-line-start-indent: 0pt
	quadding: %section-subtitle-quadding%
	keep-with-next?: #t
	(process-children))))
)

(define ($proc-section-info$ info)
  (cond ((equal? (gi) (normalize "sect1"))
	 ($sect1-info$ info))
	((equal? (gi) (normalize "sect2"))
	 ($sect2-info$ info))
	((equal? (gi) (normalize "sect3"))
	 ($sect3-info$ info))
	((equal? (gi) (normalize "sect4"))
	 ($sect4-info$ info))
	((equal? (gi) (normalize "sect5"))
	 ($sect5-info$ info))
	((equal? (gi) (normalize "section"))
	 ($section-info$ info))
	((equal? (gi) (normalize "refsect1"))
	 ($refsect1-info$ info))
	((equal? (gi) (normalize "refsect2"))
	 ($refsect2-info$ info))
	((equal? (gi) (normalize "refsect3"))
	 ($refsect3-info$ info))
	(else (empty-sosofo))))

(define ($sect1-info$ info) (empty-sosofo))
(define ($sect2-info$ info) (empty-sosofo))
(define ($sect3-info$ info) (empty-sosofo))
(define ($sect4-info$ info) (empty-sosofo))
(define ($sect5-info$ info) (empty-sosofo))
(define ($section-info$ info) (empty-sosofo))
(define ($refsect1-info$ info) (empty-sosofo))
(define ($refsect2-info$ info) (empty-sosofo))
(define ($refsect3-info$ info) (empty-sosofo))

(element sect1 ($section$))
(element (sect1 title) (empty-sosofo))

(element sect2 ($section$))
(element (sect2 title) (empty-sosofo))

(element sect3 ($section$))
(element (sect3 title) (empty-sosofo))

(element sect4 ($section$))
(element (sect4 title) (empty-sosofo))

(element sect5 ($section$))
(element (sect5 title) (empty-sosofo))

(element simplesect ($section$))
(element (simplesect title) (empty-sosofo))

