;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================ ADMONITIONS =============================

(define ($graphical-admonition$)
  (let* ((adm       (current-node))
	 (id        (attribute-string (normalize "id")))
	 (title     (select-elements (children adm) 
				     (normalize "title")))
	 (title?    (not (node-list-empty? title)))
	 (adm-title (if title?
			(with-mode title-sosofo-mode
			  (process-node-list (node-list-first title)))
			(literal (gentext-element-name adm))))
	 (gr-cell   (make element gi: "TD"
			  attributes: (list 
				       (list "WIDTH" 
					     ($admon-graphic-width$))
				       (list "ALIGN" "CENTER")
				       (list "VALIGN" "TOP"))
			  (make empty-element gi: "IMG"
				attributes: (list 
					     (list "SRC" 
						   (root-rel-path 
						    ($admon-graphic$)))
					     (list "HSPACE" "5")
					     (list "ALT"
						   (gentext-element-name adm))))))
	 (ttl-cell  (make element gi: "TH"
			  attributes: '(("ALIGN" "LEFT")
					("VALIGN" "CENTER"))
			  (make element gi: "B" adm-title)))
	 (body-cell (make element gi: "TD"
			  attributes: '(("ALIGN" "LEFT")
					("VALIGN" "TOP"))
			  (process-children))))
    (make element gi: "DIV"
	  attributes: (list (list "CLASS" (gi adm)))
	  (if id
	      (make element gi: "A"
		    attributes: (list (list "NAME" id))
		    (empty-sosofo))
	      (empty-sosofo))
	  (if %spacing-paras%
	      (make element gi: "P" (empty-sosofo))
	      (empty-sosofo))
	  (make element gi: "TABLE"
		attributes: (list (list "CLASS" (gi))
				  (list "WIDTH" ($table-width$))
				  (list "BORDER" "0"))
		(if title?
		    (make sequence
		      (make element gi: "TR"
			    gr-cell
			    ttl-cell)
		      (make element gi: "TR"
			    (make element gi: "TD"
				  (make entity-ref name: "nbsp"))
			    body-cell))
		    (make sequence
		      (make element gi: "TR"
			    gr-cell
			    body-cell)))))))

(define ($admonition$)
  (let ((id     (attribute-string (normalize "id"))))
    (if %admon-graphics%
	($graphical-admonition$)
	(make element gi: "DIV"
	      attributes: (list (list "CLASS" (gi)))
	      ;; The DIV isn't strictly necessary, of course, but it
	      ;; is consistent with the graphical-admonition case.
	      (make element gi: "BLOCKQUOTE"
		    attributes: (list
				 (list "CLASS" (gi)))
		    (if id
			(make element gi: "A"
			      attributes: (list (list "NAME" id))
			      (empty-sosofo))
			(empty-sosofo))
		    (process-children))))))

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
			  (gi (parent (current-node))))
			 (gentext-label-title-sep 
			  (gi (parent (current-node))))))))
    (make element gi: "P"
	  (if (and (not %admon-graphics%) (= (child-number) 1))
	      (make element gi: "B"
		    adm-title)
	      (empty-sosofo))
	  (process-children))))

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
			  (gi (current-node))))))
	 (id     (attribute-string (normalize "id"))))
    (if %admon-graphics%
	($graphical-admonition$)
	(make element gi: "DIV"
	      attributes: (list (list "CLASS" (gi)))
	      ;; The DIV isn't strictly necessary, of course, but it
	      ;; is consistent with the graphical-admonition case.
	      (if %spacing-paras%
		  (make element gi: "P" (empty-sosofo))
		  (empty-sosofo))
	      (make element gi: "TABLE"
		    attributes: (list
				 (list "CLASS" (gi))
				 (list "BORDER" "1")
				 (list "WIDTH" ($table-width$)))
		    (make element gi: "TR"
			  (make element gi: "TD"
				attributes: (list
					     (list "ALIGN" "CENTER"))
				(make element gi: "B"
				      (if id
					  (make element gi: "A"
						attributes: (list (list "NAME" id))
						(empty-sosofo))
					  (empty-sosofo))
				      adm-title)))
		    (make element gi: "TR"
			  (make element gi: "TD"
				attributes: (list
					     (list "ALIGN" "LEFT"))
				(process-children))))))))

(element caution ($peril$))
(element warning ($peril$))
(element (caution title) (empty-sosofo))
(element (warning title) (empty-sosofo))
