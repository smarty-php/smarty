;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================= SYNTAX DEFINITIONS =========================

(element synopsis ($verbatim-display$ %indent-synopsis-lines%
				      %number-synopsis-lines%))
(element cmdsynopsis ($paragraph$))

;; Support for ARG provided by James Bostock, augmented by norm
;;

(element (cmdsynopsis command)
  (make sequence
    (if (first-sibling? (current-node))
	(empty-sosofo)
	(make empty-element gi: "BR"))
    (next-match)
    (literal " ")))

(element group
  (let ((choice (attribute-string (normalize "choice")))
	(rep    (attribute-string (normalize "rep")))
	(sepchar (if (inherited-attribute-string (normalize "sepchar"))
		     (inherited-attribute-string (normalize "sepchar"))
		     " ")))
    (make sequence
      (if (equal? (absolute-child-number (current-node)) 1)
	  (empty-sosofo)
	  (literal sepchar))
      (cond
       ((equal? choice (normalize "plain"))
	(literal %arg-choice-plain-open-str%))
       ((equal? choice (normalize "req"))
	(literal %arg-choice-req-open-str%))
       ((equal? choice (normalize "opt"))
	(literal %arg-choice-opt-open-str%))
       (else (literal %arg-choice-def-open-str%)))
      (process-children)
      (cond
       ((equal? rep (normalize "repeat"))
	(literal %arg-rep-repeat-str%))
       ((equal? rep (normalize "norepeat"))
	(literal %arg-rep-norepeat-str%))
       (else (literal %arg-rep-def-str%)))
      (cond
       ((equal? choice (normalize "plain"))
	(literal %arg-choice-plain-close-str%))
       ((equal? choice (normalize "req"))
	(literal %arg-choice-req-close-str%))
       ((equal? choice (normalize "opt"))
	(literal %arg-choice-opt-close-str%))
       (else (literal %arg-choice-def-close-str%))))))

(element arg
  (let ((choice (attribute-string (normalize "choice")))
	(rep    (attribute-string (normalize "rep")))
	(sepchar (if (inherited-attribute-string (normalize "sepchar"))
		     (inherited-attribute-string (normalize "sepchar"))
		     " ")))
    (make sequence
      (if (equal? (absolute-child-number (current-node)) 1)
	  (empty-sosofo)
	  (literal sepchar))
      (cond
       ((equal? choice (normalize "plain"))
	(literal %arg-choice-plain-open-str%))
       ((equal? choice (normalize "req")) (literal %arg-choice-req-open-str%))
       ((equal? choice (normalize "opt")) (literal %arg-choice-opt-open-str%))
       (else (literal %arg-choice-def-open-str%)))
      (process-children)
      (cond
       ((equal? rep (normalize "repeat")) (literal %arg-rep-repeat-str%))
       ((equal? rep (normalize "norepeat")) (literal %arg-rep-norepeat-str%))
       (else (literal %arg-rep-def-str%)))
      (cond
       ((equal? choice (normalize "plain")) (literal %arg-choice-plain-close-str%))
       ((equal? choice (normalize "req")) (literal %arg-choice-req-close-str%))
       ((equal? choice (normalize "opt")) (literal %arg-choice-opt-close-str%))
       (else (literal %arg-choice-def-close-str%))))))


(element (group arg)
  (let ((choice (attribute-string (normalize "choice")))
	(rep    (attribute-string (normalize "rep"))))
    (make sequence
      (if (not (first-sibling? (current-node)))
	  (literal %arg-or-sep%)
	  (empty-sosofo))
      (process-children))))

(element sbr 
  (make empty-element gi: "BR"))

;; ----------------------------------------------------------------------
;; Syntax highlighting...

(define (funcsynopsis-function #!optional (sosofo (process-children)))
  (make element gi: "B"
	attributes: '(("CLASS" "FSFUNC"))
	sosofo))

(define (paramdef-parameter #!optional (sosofo (process-children)))
  (make element gi: "VAR"
	attributes: '(("CLASS" "PDPARAM"))
	sosofo))

;; ----------------------------------------------------------------------

(element synopfragmentref 
  (let* ((target (element-with-id (attribute-string (normalize "linkend"))))
	 (snum   (child-number target)))
    (make element gi: "I"
	  (make element gi: "A"
		attributes: (list
			     (list "HREF" (href-to target)))
		(literal "(" (number->string snum) ")"))
	  (process-children))))

(element synopfragment
  (let ((id   (element-id (current-node)))
	(snum (child-number (current-node))))
    (make element gi: "P"
	  (make element gi: "A"
		attributes: (list
			     (list "NAME" id))
		(literal "(" (number->string snum) ")"))
	  (make entity-ref name: "nbsp")
	  (process-children))))

(element funcsynopsis ($informal-object$))

(element funcsynopsisinfo ($verbatim-display$ %indent-funcsynopsisinfo-lines%
					      %number-funcsynopsisinfo-lines%))

(element funcprototype 
  (let ((paramdefs (select-elements (children (current-node)) (normalize "paramdef"))))
    (make sequence
      (make element gi: "P"
	    (make element gi: "CODE"
		  (process-children)
		  (if (equal? %funcsynopsis-style% 'kr)
		      (with-mode kr-funcsynopsis-mode
			(process-node-list paramdefs))
		      (empty-sosofo)))))))

(element funcdef
  (make element gi: "CODE"
	attributes: '(("CLASS" "FUNCDEF"))
	(process-children)))

(element (funcdef function)
  (if %funcsynopsis-decoration%
      (funcsynopsis-function)
      (process-children)))

(element void 
  (if (equal? %funcsynopsis-style% 'ansi)
      (literal "(void);")
      (literal "();")))

(element varargs (literal "(...);"))

(element paramdef
  (let ((param (select-elements (children (current-node)) (normalize "parameter"))))
    (make sequence
      (if (equal? (child-number (current-node)) 1)
	  (literal "(")
	  (empty-sosofo))
      (if (equal? %funcsynopsis-style% 'ansi)
	  (process-children)
	  (process-node-list param))
      (if (equal? (gi (ifollow (current-node))) (normalize "paramdef"))
	  (literal ", ")
	  (literal ");")))))
  
(element (paramdef parameter)
  (make sequence
    (if %funcsynopsis-decoration%
	(paramdef-parameter)
	(process-children))
    (if (equal? (gi (ifollow (current-node))) (normalize "parameter"))
	(literal ", ")
	(empty-sosofo))))

(element funcparams 
  (make sequence 
    (literal "(")
    (process-children)
    (literal ")")))

(mode kr-funcsynopsis-mode
  (element paramdef
    (make sequence
      (make empty-element gi: "BR")
      (process-children) 
      (literal ";"))))

