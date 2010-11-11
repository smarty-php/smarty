;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================ ADMONITIONS =============================

(define ($graphical-admonition$)
  (let* ((adm       (current-node))
	 (title     (select-elements (children adm) 
				     (normalize "title")))
	 (title?    (not (node-list-empty? title)))
	 (adm-title (if title?
			(with-mode title-sosofo-mode
			  (process-node-list (node-list-first title)))
			(literal (gentext-element-name adm))))
	 (graphic   (make external-graphic
		      display?: #f
		      entity-system-id: ($admon-graphic$)))
	 (f-child   (node-list-first (children (current-node))))
	 (r-child   (node-list-rest (children (current-node)))))
    (make display-group
      space-before: %block-sep%
      space-after: %block-sep%
      start-indent: (+ (inherited-start-indent) ($admon-graphic-width$))
      font-family-name: %admon-font-family%
      font-size: (- %bf-size% 1pt)
      font-weight: 'medium
      font-posture: 'upright
      line-spacing: (* (- %bf-size% 1pt) %line-spacing-factor%)
      (if title?
	  (make display-group
	    (make paragraph
	      first-line-start-indent: (- ($admon-graphic-width$))
	      (make line-field
		field-width: ($admon-graphic-width$)
		graphic)
	      (make sequence
		font-family-name: %title-font-family%
		font-weight: 'bold
		adm-title))
	    (process-children))
	  (make display-group
	    (make paragraph
	      first-line-start-indent: (- ($admon-graphic-width$))
	      (make line-field
		field-width: ($admon-graphic-width$)
		graphic)
	      (process-node-list (children f-child)))
	    (process-node-list r-child))))))

(define ($admonition$)
  (if %admon-graphics%
      ($graphical-admonition$)
      (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	start-indent: (if %admon-graphics%
			  (inherited-start-indent)
			  (+ (inherited-start-indent) (* (ILSTEP) 2)))
	font-size: (- %bf-size% 1pt)
	font-weight: 'medium
	font-posture: 'upright
	font-family-name: %admon-font-family%
	line-spacing: (* (- %bf-size% 1pt) %line-spacing-factor%)
	(process-children))))

(define ($admonpara$)
  (let* ((title     (select-elements 
		     (children (parent (current-node))) (normalize "title")))
	 (has-title (not (node-list-empty? title)))
	 (adm-title (if has-title 
			(make sequence
			  (with-mode title-sosofo-mode
			    (process-node-list (node-list-first title)))
			  (literal (gentext-label-title-sep 
				    (gi (parent (current-node))))))
			(literal
			 (gentext-element-name 
			  (parent (current-node)))
			 (gentext-label-title-sep 
			  (gi (parent (current-node))))))))
    (make paragraph
      space-before: %para-sep%
      space-after: %para-sep%
      (if (and (not %admon-graphics%) (= (child-number) 1))
	  (make sequence
	    font-family-name: %title-font-family%
	    font-weight: 'bold
	    adm-title)
	  (empty-sosofo))
      (process-children-trim))))

(element important ($admonition$))
(element (important title) (empty-sosofo))
(element (important para) ($admonpara$))
(element (important simpara) ($admonpara$))

(element note ($admonition$))
(element (note title) (empty-sosofo))
(element (note para) ($admonpara$))
(element (note simpara) ($admonpara$))

(element tip ($admonition$))
(element (tip title) (empty-sosofo))
(element (tip para) ($admonpara$))
(element (tip simpara) ($admonpara$))

;; perils are given special treatment by generating a centered title
;;   and throwing a box around them
;; note that the paragraph indents are set by the box characteristics
;;
(define ($peril$)
  (let* ((title     (select-elements 
		     (children (current-node)) (normalize "title")))
	 (has-title (not (node-list-empty? title)))
	 (adm-title (if has-title 
			(make sequence
			  (with-mode title-sosofo-mode
			    (process-node-list (node-list-first title))))
			(literal
			 (gentext-element-name 
			  (current-node)))))
	 (hs (HSIZE 2)))
  (if %admon-graphics%
      ($graphical-admonition$)
      (make display-group
	space-before: %block-sep%
	space-after: %block-sep%
	font-family-name: %admon-font-family%
	font-size: (- %bf-size% 1pt)
	font-weight: 'medium
	font-posture: 'upright
	line-spacing: (* (- %bf-size% 1pt) %line-spacing-factor%)
	(make box
	  display?: #t
	  box-type: 'border
	  line-thickness: 2pt
	  start-indent: (+ (inherited-start-indent) (* 2 (ILSTEP)) 2pt)
	  end-indent: (inherited-end-indent)
	  (make paragraph
	    space-before: %para-sep%
	    space-after: %para-sep%
	    start-indent: 1em
	    end-indent: 1em
	    font-family-name: %title-font-family%
	    font-weight: 'bold
	    font-size: hs
	    line-spacing: (* hs %line-spacing-factor%)
	    quadding: 'center
	    keep-with-next?: #t
	    adm-title)
	  (process-children))))))

(element caution ($peril$))
(element (caution title) (empty-sosofo))

(element warning ($peril$))
(element (warning title) (empty-sosofo))
