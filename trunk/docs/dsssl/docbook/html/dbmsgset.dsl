;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ======================== ERROR MESSAGES (ETC.) =======================

(element msgset (process-children))

(element msgentry ($block-container$))

(element simplemsgentry ($block-container$))

(element msg ($block-container$))

(element msgmain (process-children))

(element msgsub
  (process-children))

(element msgrel (empty-sosofo))

(element msgtext (process-children))

(element msginfo ($indent-para-container$))

(define ($genhead-para$ headtext)
  (make element gi: "P"
	(make element gi: "B"
	      (literal
	       (string-append headtext ": ")))
	(process-children)))

(element msglevel ($genhead-para$ (gentext-element-name (current-node))))
(element msgorig ($genhead-para$ (gentext-element-name (current-node))))
(element msgaud ($genhead-para$ (gentext-element-name (current-node))))

(element msgexplan ($indent-para-container$))
(element (msgexplan title) ($runinhead$))
(element (msgexplan para) (make sequence (process-children)))

