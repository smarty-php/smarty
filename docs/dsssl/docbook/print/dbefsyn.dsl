;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================ CLASS SYNOPSIS =============================

(define %indent-classsynopsisinfo-lines% #f)
(define %number-classsynopsisinfo-lines% #f)

(define %default-classsynopsis-language% "java")

(element classsynopsis
  (let ((language (if (attribute-string (normalize "language"))
		      (attribute-string (normalize "language"))
		      %default-classsynopsis-language%)))
    (case language
      (("java") (with-mode cs-java-mode
		  (process-node-list (current-node))))
      (("perl") (with-mode cs-perl-mode
		  (process-node-list (current-node))))
      (("idl") (with-mode cs-idl-mode
		  (process-node-list (current-node))))
      (("python") (with-mode cs-python-mode
		    (process-node-list (current-node))))
      (else (with-mode cs-java-mode
	      (process-node-list (current-node)))))))

(element methodsynopsis
  (let ((language (if (attribute-string (normalize "language"))
		      (attribute-string (normalize "language"))
		      %default-classsynopsis-language%)))
    (case language
      (("java") (with-mode cs-java-mode
		  (process-node-list (current-node))))
      (("perl") (with-mode cs-perl-mode
		  (process-node-list (current-node))))
      (("idl") (with-mode cs-idl-mode
		  (process-node-list (current-node))))
      (("python") (with-mode cs-python-mode
		    (process-node-list (current-node))))
      (else (with-mode cs-java-mode
	      (process-node-list (current-node)))))))

(element fieldsynopsis
  (let ((language (if (attribute-string (normalize "language"))
		      (attribute-string (normalize "language"))
		      %default-classsynopsis-language%)))
    (case language
      (("java") (with-mode cs-java-mode
		  (process-node-list (current-node))))
      (("perl") (with-mode cs-perl-mode
		  (process-node-list (current-node))))
      (("idl") (with-mode cs-idl-mode
		  (process-node-list (current-node))))
      (("python") (with-mode cs-python-mode
		    (process-node-list (current-node))))
      (else (with-mode cs-java-mode
	      (process-node-list (current-node)))))))

(element constructorynopsis
  (let ((language (if (attribute-string (normalize "language"))
		      (attribute-string (normalize "language"))
		      %default-classsynopsis-language%)))
    (case language
      (("java") (with-mode cs-java-mode
		  (process-node-list (current-node))))
      (("perl") (with-mode cs-perl-mode
		  (process-node-list (current-node))))
      (("idl") (with-mode cs-idl-mode
		  (process-node-list (current-node))))
      (("python") (with-mode cs-python-mode
		    (process-node-list (current-node))))
      (else (with-mode cs-java-mode
	      (process-node-list (current-node)))))))

(element destructorsynopsis
  (let ((language (if (attribute-string (normalize "language"))
		      (attribute-string (normalize "language"))
		      %default-classsynopsis-language%)))
    (case language
      (("java") (with-mode cs-java-mode
		  (process-node-list (current-node))))
      (("perl") (with-mode cs-perl-mode
		  (process-node-list (current-node))))
      (("idl") (with-mode cs-idl-mode
		  (process-node-list (current-node))))
      (("python") (with-mode cs-python-mode
		    (process-node-list (current-node))))
      (else (with-mode cs-java-mode
	      (process-node-list (current-node)))))))

;; ===== Java ========================================================

(mode cs-java-mode

(element classsynopsis
  (let* ((classes      (select-elements (children (current-node))
					(normalize "ooclass")))
	 (classname    (node-list-first classes))
	 (superclasses (node-list-rest classes)))
  (make display-group
    use: verbatim-style
    (make paragraph
      (process-node-list classname)
      (process-node-list superclasses)
      (literal "{"))
    (process-node-list
     (node-list-filter-by-gi
      (children (current-node))
      (list (normalize "constructorsynopsis")
	    (normalize "destructorsynopsis")
	    (normalize "fieldsynopsis")
	    (normalize "methodsynopsis")
	    (normalize "classsynopsisinfo"))))
    (make paragraph
      (literal "}")))))

(element classsynopsisinfo
  ($verbatim-display$ %indent-classsynopsisinfo-lines%
		      %number-classsynopsisinfo-lines%))

(element ooclass
  (make sequence
    (if (first-sibling?)
	(literal " ")
	(literal ", "))
    (process-children)))

(element oointerface
  (make sequence
    (if (first-sibling?)
	(literal " ")
	(literal ", "))
    (process-children)))

(element ooexception
  (make sequence
    (if (first-sibling?)
	(literal " ")
	(literal ", "))
    (process-children)))

(element modifier
  (make sequence
    (process-children)
    (literal " ")))

(element classname
  (if (first-sibling?)
      (make sequence
	(literal "class ")
	(process-children)
	(literal " ")
	(if (last-sibling?)
	    (empty-sosofo)
	    (literal "extends ")))
      (make sequence
	(process-children)
	(if (last-sibling?)
	    (literal " ")
	    (literal ", ")))))

(element fieldsynopsis
  (make paragraph
    use: inline-verbatim-style
    (literal "  ")
    (process-children)
    (literal ";")))

(element type
  (make sequence
    (process-children)
    (literal " ")))

(element varname
  (make sequence
    (process-children)))

(element initializer
  (make sequence
    (literal " = ")
    (process-children)))

(element constructorsynopsis
  (java-method-synopsis))

(element destructorsynopsis
  (java-method-synopsis))

(element methodsynopsis
  (java-method-synopsis))

(element void
  (literal "void "))

(element methodname
  (process-children))

(element methodparam
  (make sequence
    (if (first-sibling?)
	(empty-sosofo)
	(literal ", "))
    (process-children)))

(element parameter
  (process-children))

(element exceptionname
  (make sequence
    (if (first-sibling?)
	(literal " throws ")
	(literal ", "))
    (process-children)))
)

(define (java-method-synopsis #!optional (nd (current-node)))
  (let* ((modifiers  (select-elements (children nd)
				      (normalize "modifier")))
	 (notmod     (node-list-filter-by-not-gi
		      (children nd)
		      (list (normalize "modifier"))))
	 (type       (if (equal? (gi (node-list-first notmod)) 
				 (normalize "methodname"))
			 (empty-node-list)
			 (node-list-first notmod)))
	 (methodname (select-elements (children nd)
				      (normalize "methodname")))
	 (param      (node-list-filter-by-gi (node-list-rest notmod)
					     (list (normalize "methodparam"))))
	 (excep      (select-elements (children nd)
				      (normalize "exceptionname"))))
    (make paragraph
      use: inline-verbatim-style
      (literal "  ")
      (process-node-list modifiers)
      (process-node-list type)
      (process-node-list methodname)
      (literal "(")
      (process-node-list param)
      (literal ")")
      (process-node-list excep)
      (literal ";"))))

;; ===== Perl ========================================================

(mode cs-perl-mode

(element classsynopsis
  (let* ((modifiers    (select-elements (children (current-node))
					(normalize "modifier")))
	 (classes      (select-elements (children (current-node))
					(normalize "classname")))
	 (classname    (node-list-first classes))
	 (superclasses (node-list-rest classes)))
  (make display-group
    use: verbatim-style;
    (make paragraph
      (literal "package ")
      (process-node-list classname)
      (literal ";"))
    (if (node-list-empty? superclasses)
	(empty-sosofo)
	(make sequence
	  (literal "@ISA = (");
	  (process-node-list superclasses)
	  (literal ";")))
    (process-node-list
     (node-list-filter-by-gi
      (children (current-node))
      (list (normalize "constructorsynopsis")
	    (normalize "destructorsynopsis")
	    (normalize "fieldsynopsis")
	    (normalize "methodsynopsis")
	    (normalize "classsynopsisinfo")))))))

(element classsynopsisinfo
  ($verbatim-display$ %indent-classsynopsisinfo-lines%
		      %number-classsynopsisinfo-lines%))

(element modifier
  (literal "Perl ClassSynopses don't use Modifiers"))

(element classname
  (if (first-sibling?)
      (process-children)
      (make sequence
	(process-children)
	(if (last-sibling?)
	    (empty-sosofo)
	    (literal ", ")))))

(element fieldsynopsis
  (make paragraph
    use: inline-verbatim-style
    (literal "  ");
    (process-children)
    (literal ";")))

(element type
  (make sequence
    (process-children)
    (literal " ")))

(element varname
  (make sequence
    (process-children)))

(element initializer
  (make sequence
    (literal " = ")
    (process-children)))

(element constructorsynopsis
  (perl-method-synopsis))

(element destructorsynopsis
  (perl-method-synopsis))

(element methodsynopsis
  (perl-method-synopsis))

(element void
  (empty-sosofo))

(element methodname
  (process-children))

(element methodparam
  (make sequence
    (if (first-sibling?)
	(empty-sosofo)
	(literal ", "))
    (process-children)))

(element parameter
  (process-children))

(element exceptionname
  (literal "Perl ClassSynopses don't use Exceptions"))

)

(define (perl-method-synopsis #!optional (nd (current-node)))
  (let* ((modifiers  (select-elements (children nd)
				      (normalize "modifier")))
	 (notmod     (node-list-filter-by-not-gi
		      (children nd)
		      (list (normalize "modifier"))))
	 (type       (if (equal? (gi (node-list-first notmod)) 
				 (normalize "methodname"))
			 (empty-node-list)
			 (node-list-first notmod)))
	 (methodname (select-elements (children nd)
				      (normalize "methodname")))
	 (param      (node-list-filter-by-gi (node-list-rest notmod)
					     (list (normalize "type")
						   (normalize "void"))))
	 (excep      (select-elements (children nd)
				      (normalize "exceptionname"))))
    (make paragraph
      use: inline-verbatim-style
      (literal "sub ")
      (process-node-list modifiers)
      (process-node-list type)
      (process-node-list methodname)
      (literal " { ... }"))))

;; ===== IDL =========================================================

(mode cs-idl-mode

(element classsynopsis
  (let* ((modifiers    (select-elements (children (current-node))
					(normalize "modifier")))
	 (classes      (select-elements (children (current-node))
					(normalize "classname")))
	 (classname    (node-list-first classes))
	 (superclasses (node-list-rest classes)))
  (make display-group
    use: verbatim-style;
    (make paragraph
      (literal "interface ")
      (process-node-list modifiers)
      (process-node-list classname)
      (if (node-list-empty? superclasses)
	  (literal " ")
	  (make sequence
	    (literal " : ")
	    (process-node-list superclasses)))
      (literal "{"))
    (process-node-list
     (node-list-filter-by-gi
      (children (current-node))
      (list (normalize "constructorsynopsis")
	    (normalize "destructorsynopsis")
	    (normalize "fieldsynopsis")
	    (normalize "methodsynopsis")
	    (normalize "classsynopsisinfo"))))
    (make paragraph
      (literal "}")))))

(element classsynopsisinfo
  ($verbatim-display$ %indent-classsynopsisinfo-lines%
		      %number-classsynopsisinfo-lines%))

(element modifier
  (make sequence
    (process-children)
    (literal " ")))

(element classname
  (if (first-sibling?)
      (process-children)
      (make sequence
	(process-children)
	(if (last-sibling?)
	    (empty-sosofo)
	    (literal ", ")))))

(element fieldsynopsis
  (make paragraph
    use: inline-verbatim-style
    (literal "  ");
    (process-children)
    (literal ";")))

(element type
  (make sequence
    (process-children)
    (literal " ")))

(element varname
  (make sequence
    (process-children)))

(element initializer
  (make sequence
    (literal " = ")
    (process-children)))

(element constructorsynopsis
  (idl-method-synopsis))

(element destructorsynopsis
  (idl-method-synopsis))

(element methodsynopsis
  (idl-method-synopsis))

(element void
  (literal "void "))

(element methodname
  (process-children))

(element methodparam
  (make sequence
    (if (first-sibling?)
	(empty-sosofo)
	(literal ", "))
    (process-children)))

(element parameter
  (process-children))

(element exceptionname
  (make sequence
    (if (first-sibling?)
	(literal " raises(")
	(literal ", "))
    (process-children)
    (if (last-sibling?)
	(literal ")")
	(empty-sosofo))))
)

(define (idl-method-synopsis #!optional (nd (current-node)))
  (let* ((modifiers  (select-elements (children nd)
				      (normalize "modifier")))
	 (notmod     (node-list-filter-by-not-gi
		      (children nd)
		      (list (normalize "modifier"))))
	 (type       (if (equal? (gi (node-list-first notmod)) 
				 (normalize "methodname"))
			 (empty-node-list)
			 (node-list-first notmod)))
	 (methodname (select-elements (children nd)
				      (normalize "methodname")))
	 (param      (node-list-filter-by-gi (node-list-rest notmod)
					     (list (normalize "methodparam"))))
	 (excep      (select-elements (children nd)
				      (normalize "exceptionname"))))
    (make paragraph
      use: inline-verbatim-style
      (process-node-list modifiers)
      (process-node-list type)
      (process-node-list methodname)
      (literal "(")
      (process-node-list param)
      (literal ")")
      (process-node-list excep)
      (literal ";"))))

;; ===== Python ======================================================
;; Contributed by Lane Stevens, lane@cycletime.com

(mode cs-python-mode
  (element classsynopsis
    (let* ((classes      (select-elements (children (current-node))
					  (normalize "ooclass")))
	   (classname    (node-list-first classes))
	   (superclasses (node-list-rest classes)))
      (make display-group
	use: verbatim-style
	(make paragraph
	  (literal "class ")
	  (process-node-list classname)
	  (literal "(")
	  (process-node-list superclasses)
	  (literal ") :"))
	(process-node-list
	 (node-list-filter-by-gi
	  (children (current-node))
	  (list (normalize "constructorsynopsis")
		(normalize "destructorsynopsis")
		(normalize "fieldsynopsis")
		(normalize "methodsynopsis")
		(normalize "classsynopsisinfo"))))
	)
      )
    )

  (element ooclass
    (make sequence
      (process-children)
      (cond
       ((first-sibling?) (literal " "))
       ((last-sibling?) (empty-sosofo))
       (#t (literal ", "))
       )
      )
    )
  
  (element methodsynopsis
    (python-method-synopsis))
  
  (element classname
    (process-children))

  (element initializer
    (make sequence
      (literal " = ")
      (process-children)))

  (element methodname
    (process-children))

  (element methodparam
    (make sequence
      (process-children)
      (if (last-sibling?)
	  (empty-sosofo)
	  (literal ", "))
      )
    )

  (element parameter
    (process-children))
  )

(define (python-method-synopsis #!optional (nd (current-node)))
  (let* ((the-method-name (select-elements (children nd) (normalize "methodname")))
	 (the-method-params (select-elements (children nd) (normalize "methodparam")))
	 )
    (make paragraph
      use: inline-verbatim-style
      (literal "    def ")
      (process-node-list the-method-name)
      (literal "(")
      (process-node-list the-method-params)
      (literal ") :"))
    )
  )

;; EOF
