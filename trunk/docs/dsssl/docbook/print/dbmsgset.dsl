;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ======================== ERROR MESSAGES (ETC.) =======================

(element msgset (process-children))

(element msgentry ($informal-object$))

(element simplemsgentry ($informal-object$))

(element msg
  (make display-group
	font-weight: 'bold
	font-family-name: %mono-font-family%
	(process-children)))

(element msgmain (process-children))

(element msgsub
  (make display-group
	start-indent: (+ (inherited-start-indent) (ILSTEP))
	(process-children)))

(element msgrel (empty-sosofo))

(element msgtext (process-children))

(element msginfo ($indent-para-container$))

(define ($genhead-para$ headtext)
  (make paragraph
	space-before: %para-sep%
	space-after: %para-sep%
	(make sequence
	      font-weight: 'bold
	      (literal
	        (string-append headtext ": ")))
	(process-children)))

(element msglevel ($genhead-para$ (gentext-element-name (current-node))))
(element msgorig ($genhead-para$ (gentext-element-name (current-node))))
(element msgaud ($genhead-para$ (gentext-element-name (current-node))))

(element msgexplan ($indent-para-container$))
(element (msgexplan title) ($runinhead$))
(element (msgexplan para) (make sequence (process-children)))

