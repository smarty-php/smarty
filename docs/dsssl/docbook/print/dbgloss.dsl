;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================= GLOSSARY ELEMENTS ==========================

(element glossary ($component$))
(element (article glossary) ($section$)) ;; this is a special case
(element (glossary title) (empty-sosofo))

(element glossdiv ($section$))
(element (glossdiv title) (empty-sosofo))

(element glosslist ($block-container$))
(element glossentry  (process-children))

;; a glossentry glossterm
(element (glossentry glossterm) ($lowtitle$ 3 2))
(element (glossdiv glossentry glossterm) ($lowtitle$ 3 3))
(element (glossentry acronym) (empty-sosofo))
(element (glossentry abbrev) (empty-sosofo))
(element glossdef ($indent-para-container$))

(element glosssee ($italic-seq$))

(element (glossentry glosssee)
  (let ((otherterm (attribute-string (normalize "otherterm"))))
    (make paragraph
      space-before: %para-sep%
      space-after: %para-sep%
      start-indent: (+ (inherited-start-indent) (* (ILSTEP) 2))
      quadding: %default-quadding%
      ($italic-seq$ (literal (gentext-element-name (current-node))
			     (gentext-label-title-sep (gi))))
      (if otherterm
	  (make link 
	    destination: (node-list-address (element-with-id otherterm))
	    (with-mode otherterm
	      (process-element-with-id otherterm)))
	  (process-children)))))

;; When we hit the first GLOSSSEEALSO, process all of them as a node-list
(element glossseealso
  (if (first-sibling?)
      (make paragraph
	($italic-seq$ (literal (gentext-element-name (current-node))
			       (gentext-label-title-sep (gi))))
	(with-mode glossseealso
	  (process-node-list
	   (select-elements (children (parent)) '(glossseealso))))
	(literal "."))
      (empty-sosofo)))

(mode glossseealso

  (element glossseealso
    (let ((otherterm (attribute-string (normalize "otherterm"))))
      (make sequence
	(if (first-sibling?)
	    (empty-sosofo)
	    ($italic-seq$ (literal ", ")))

	(if otherterm ;; but this should be required...
	    (make link 
	      destination: (node-list-address (element-with-id otherterm))
	      (with-mode otherterm
		(process-element-with-id otherterm)))
	    (process-children)))))

)

;; This is referenced within the GLOSSSEE and GLOSSSEEALSO element
;; construction expressions.  The OTHERTERM attributes on GLOSSSEE and
;; GLOSSSEEALSO (should) refer to GLOSSENTRY elements but we're only
;; interested in the text within the GLOSSTERM.  Discard the revision
;; history and the definition from the referenced term.
(mode otherterm
  (element glossentry
    (process-children))
  (element glossterm
    (process-children))
  (element glossdef
    (empty-sosofo))
  (element revhistory
    (empty-sosofo))
  (element glosssee
    (empty-sosofo))
  (element (glossentry acronym)
    (empty-sosofo))
  (element (glossentry abbrev)
    (empty-sosofo)))

;; an inline gloss term
(element glossterm
  (let* ((linkend   (attribute-string (normalize "linkend"))))
    (if linkend
	(make link 
	  destination: (node-list-address (element-with-id linkend))
	  ($italic-seq$))
	($italic-seq$))))

;; a first glossterm
(element firstterm
  (let* ((linkend (attribute-string (normalize "linkend")))
	 (sosofo  (if linkend
		      (make link 
			destination: (node-list-address
				      (element-with-id linkend))
			($italic-seq$))
		      ($italic-seq$))))
    (if firstterm-bold
	(make sequence
	  font-weight: 'bold
	  sosofo)
	sosofo)))
