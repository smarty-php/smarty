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
  (make paragraph
    space-before: 0pt
    space-after: 0pt
    ($img$ (current-node) #t)))

(element informalequation
  ;; Derived from informal-object
  (let ((rule-before? %informalequation-rules%)
	(rule-after? %informalequation-rules%))
    (if %equation-autolabel%
	(make display-group
	  space-before: %block-sep%
	  space-after:  %block-sep%
	  start-indent: (+ %block-start-indent% 
			   (inherited-start-indent))
	  keep-with-next?: (object-title-after)

	  (if rule-before?
	      (make rule
		orientation: 'horizontal
		line-thickness: %object-rule-thickness%
		display-alignment: 'center
		space-after: (/ %block-sep% 2)
		keep-with-next?: #t)
	      (empty-sosofo))

	  (make table
	    (make table-column
	      column-number: 1
	      width: (- %text-width% 
			(+ (inherited-start-indent) 
			   (inherited-end-indent) 
			   1in)))
	    (make table-column
	      column-number: 2
	      width: 1in)
	    (make table-row
	      (make table-cell
		cell-row-alignment: 'center
		start-indent: 0pt
		end-indent: 0pt
		(process-children))
	      (make table-cell
		cell-row-alignment: 'center
		quadding: 'end
		start-indent: 0pt
		end-indent: 0pt
		(make paragraph
		  (literal "(" (element-label (current-node)) ")")))))

	  (if rule-after?
	      (make rule
		orientation: 'horizontal
		line-thickness: %object-rule-thickness%
		display-alignment: 'center
		space-before: (/ %block-sep% 2)
		keep-with-previous?: #t)
	      (empty-sosofo)))
	($informal-object$ rule-before? rule-after?))))

(element (informalequation alt) (empty-sosofo))
(element (informalequation graphic) 
  (make paragraph
    space-before: 0pt
    space-after: 0pt
    quadding: 'end
    ($img$ (current-node) #t)))

(element inlineequation ($inline-object$))
(element (inlineequation alt) (empty-sosofo))
(element (inlineequation graphic) 
  (make sequence
    ($img$ (current-node) #f)))

