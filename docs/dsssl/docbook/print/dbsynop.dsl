;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================= SYNTAX DEFINITIONS =========================

(element synopsis ($verbatim-display$ %indent-synopsis-lines%
				      %number-synopsis-lines%))

(element cmdsynopsis
  ;; Can't be an $informal-object$ because it needs the paragraph
  ;; wrapper around process-children
  (make display-group
    start-indent: (+ %block-start-indent% (inherited-start-indent))
    space-before: %block-sep%
    space-after: %block-sep%
    (make paragraph
      (process-children))))

;; Support for ARG provided by James Bostock, augmented by norm
;;

(element (cmdsynopsis command)
  (make sequence
    (if (first-sibling? (current-node))
	(empty-sosofo)
	(make paragraph-break))
    (next-match)
    (literal " ")))

(element group
  (let ((choice  (attribute-string (normalize "choice")))
	(rep     (attribute-string (normalize "rep")))
	(sepchar (if (inherited-attribute-string (normalize "sepchar"))
		     (inherited-attribute-string (normalize "sepchar"))
		     " ")))
    (make sequence
      (if (equal? (absolute-child-number (current-node)) 1)
	  (empty-sosofo)
	  (literal sepchar))
      (cond
       ((equal? choice (normalize "plain")) (literal %arg-choice-plain-open-str%))
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

(element arg
  (let ((choice  (attribute-string (normalize "choice")))
	(rep     (attribute-string (normalize "rep")))
	(sepchar (if (inherited-attribute-string (normalize "sepchar"))
		     (inherited-attribute-string (normalize "sepchar"))
		     " ")))
    (make sequence
      (if (equal? (absolute-child-number (current-node)) 1)
	  (empty-sosofo)
	  (literal sepchar))
      (cond
       ((equal? choice (normalize "plain")) (literal %arg-choice-plain-open-str%))
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
	(rep (attribute-string (normalize "rep"))))
    (make sequence
      (if (not (first-sibling? (current-node)))
	  (literal %arg-or-sep%)
	  (empty-sosofo))
      (process-children))))

(element sbr 
  (make paragraph-break))

;; ----------------------------------------------------------------------
;; Syntax highlighting...

(define (funcsynopsis-function #!optional (sosofo (process-children)))
  (make sequence
    font-weight: 'bold
    sosofo))

(define (paramdef-parameter #!optional (sosofo (process-children)))
  (make sequence
    font-posture: 'italic
    sosofo))

;; ----------------------------------------------------------------------

(element synopfragmentref 
  (let* ((target (element-with-id (attribute-string (normalize "linkend"))))
	 (snum   (child-number target)))
    (make sequence
      font-posture: 'italic
      (make link 
	destination: (node-list-address target)
	(make sequence
	  font-posture: 'upright
	  ($callout-bug$ snum)))
      (process-children))))

(element synopfragment
  (let ((snum (child-number (current-node))))
    (make paragraph
      ($callout-bug$ snum)
      (literal " ")
      (process-children))))

(element funcsynopsis 
  (let* ((width-in-chars (if (attribute-string "width")
			     (string->number (attribute-string "width"))
			     %verbatim-default-width%))
	 (fsize (lambda () (if (or (attribute-string (normalize "width"))
				   (not %verbatim-size-factor%))
			       (/ (/ (- %text-width% (inherited-start-indent))
				     width-in-chars) 
				  0.7)
			       (* (inherited-font-size) 
				  %verbatim-size-factor%)))))
    ;; This used to be a sequence, but that caused the start-indent to be
    ;; wrong when it was the first element of a RefSect.  Making it a
    ;; paragraph makes the bug go away and doesn't seem to have any ill
    ;; effects.  Need to investigate further...
    (make paragraph
      font-family-name: %mono-font-family%
      font-size: (fsize)
      font-weight: 'medium
      font-posture: 'upright
      line-spacing: (* (fsize) %line-spacing-factor%)
      ($informal-object$))))

(element funcsynopsisinfo 
  ;; Fake out the font-size so that when verbatim-display calculates the
  ;; verbatim-size-factor it doesn't get squared.  This will fail if the
  ;; "correct" size isn't bfsize, but what can I do?
  (make sequence
    font-size: %bf-size%
    ($verbatim-display$ %indent-funcsynopsisinfo-lines%
			%number-funcsynopsisinfo-lines%)))

(element funcprototype 
  (let ((paramdefs (select-elements (children (current-node))
				    (normalize "paramdef"))))
    (make sequence
      (make paragraph
	font-family-name: %mono-font-family%
	(process-children))
      (if (equal? %funcsynopsis-style% 'kr)
	  (with-mode kr-funcsynopsis-mode
	    (process-node-list paramdefs))
	  (empty-sosofo)))))

(element funcdef (process-children))
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
      (make paragraph
	font-family-name: %mono-font-family%
	start-indent: (+ (inherited-start-indent) %kr-funcsynopsis-indent%)
	(make sequence
	  (process-children) 
	  (literal ";"))))))
