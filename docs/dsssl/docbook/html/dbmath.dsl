;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define %equation-autolabel% #f)

(element equation
  ;; derived from $semiformal-object$
  (if (node-list-empty? (select-elements (children (current-node))
					 (normalize "title")))
      ($informal-object$ %informalequation-rules% %informalequation-rules%)
      ($formal-object$ %informalequation-rules% %informalequation-rules%)))

(element (equation title) (empty-sosofo))
(element (equation alt) (empty-sosofo))
(element (equation graphic)
  (let ((alttag (select-elements (children (parent)) (normalize "alt"))))
    (if alttag
	($img$ (current-node) (data alttag))
	($img$))))

(element informalequation
  ;; Derived from informal-object
  (let ((rule-before? %informalequation-rules%)
	(rule-after? %informalequation-rules%))
    (if %equation-autolabel%
	(make sequence
	  (if rule-before?
	      (make empty-element gi: "HR")
	      (empty-sosofo))
	  (make element gi: "TABLE"
		attributes: '(("CLASS" "INFORMALEQUATION")
			      ("WIDTH" "100%")
			      ("BORDER" "0"))
		(make element gi: "TR"
		      (make element gi: "TD"
			    attributes: '(("VALIGN" "MIDDLE")
					  ("ALIGN" "LEFT"))
			    (process-children))
		      (make element gi: "TD"
			    attributes: '(("VALIGN" "MIDDLE")
					  ("ALIGN" "RIGHT")
					  ("WIDTH" "100"))
			    (literal "(" 
				     (element-label (current-node)) 
				     ")"))))
	  (if rule-after?
	      (make empty-element gi: "HR")
	      (empty-sosofo)))
	($informal-object$ rule-before? rule-after?))))

(element (informalequation alt) (empty-sosofo))
(element (informalequation graphic) 
  (let ((alttag (select-elements (children (parent)) (normalize "alt"))))
    (if alttag
	($img$ (current-node) (data alttag))
	($img$))))

(element inlineequation ($inline-object$))
(element (inlineequation alt) (empty-sosofo))
(element (inlineequation graphic) 
  (let ((alttag (select-elements (children (parent)) (normalize "alt"))))
    (if alttag
	($img$ (current-node) (data alttag))
	($img$))))
