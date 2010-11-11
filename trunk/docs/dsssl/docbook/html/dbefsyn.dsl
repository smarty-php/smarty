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
      (("cpp") (with-mode cs-cpp-mode
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
      (("cpp") (with-mode cs-cpp-mode
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
      (("cpp") (with-mode cs-cpp-mode
		  (process-node-list (current-node))))
      (("python") (with-mode cs-python-mode
		    (process-node-list (current-node))))
      (else (with-mode cs-java-mode
	      (process-node-list (current-node)))))))

(element constructorsynopsis
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
      (("cpp") (with-mode cs-cpp-mode
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
      (("cpp") (with-mode cs-cpp-mode
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
    (make element gi: "pre"
	  attributes: '(("class" "classsynopsis"))
	  (process-node-list classname)
	  (process-node-list superclasses)
	  (literal "{&#RE;")
	  (process-node-list
	   (node-list-filter-by-gi
	    (children (current-node))
	    (list (normalize "constructorsynopsis")
		  (normalize "destructorsynopsis")
		  (normalize "fieldsynopsis")
		  (normalize "methodsynopsis")
		  (normalize "classsynopsisinfo"))))
	  (literal "}"))))

(element classsynopsisinfo
  ($verbatim-display$ %indent-classsynopsisinfo-lines%
		      %number-classsynopsisinfo-lines%))

(element ooclass
  (make sequence
    (if (first-sibling?)
	(literal " ")
	(literal ", "))
    (make element gi: "SPAN"
	  attributes: '(("class" "ooclass"))
	  (process-children))))

(element oointerface
  (make sequence
    (if (first-sibling?)
	(literal " ")
	(literal ", "))
    (make element gi: "SPAN"
	  attributes: '(("class" "oointerface"))
	  (process-children))))

(element ooexception
  (make sequence
    (if (first-sibling?)
	(literal " ")
	(literal ", "))
    (make element gi: "SPAN"
	  attributes: '(("class" "ooexception"))
	  (process-children))))

(element modifier
  (make element gi: "span"
	attributes: '(("class" "modifier"))
	(process-children)
	(literal " ")))

(element classname
  (if (first-sibling?)
      (make sequence
	(literal "class ")
	(make element gi: "span"
	      attributes: '(("class" "classname"))
	      (process-children)
	      (literal " "))
	(if (last-sibling?)
	    (empty-sosofo)
	    (literal "extends ")))
      (make sequence
	(make element gi: "span"
	      attributes: '(("class" "superclass"))
	      (process-children))
	(if (last-sibling?)
	    (literal " ")
	    (literal ", ")))))

(element fieldsynopsis
  (make element gi: "code"
	attributes: '(("class" "fieldsynopsis"))
	(literal "  ")
	(process-children)
	(literal ";&#RE;")))

(element type
  (make element gi: "span"
	attributes: '(("class" "type"))
	(process-children)
	(literal " ")))

(element varname
  (make element gi: "span"
	attributes: '(("class" "varname"))
	(process-children)))

(element initializer
  (make element gi: "span"
	attributes: '(("class" "initializer"))
	(literal " = ")
	(process-children)))

(element constructorsynopsis
  (java-method-synopsis))

(element destructorsynopsis
  (java-method-synopsis))

(element methodsynopsis
  (java-method-synopsis))

(element void
  (make element gi: "span"
	attributes: '(("class" "void"))
	(literal "void ")))

(element methodname
  (process-children))

(element methodparam
  (make element gi: "span"
	attributes: '(("class" "methodparam"))
	(if (first-sibling?)
	    (empty-sosofo)
	    (literal ", "))
	(process-children)))

(element parameter
  (make element gi: "span"
	attributes: '(("class" "parameter"))
	(process-children)))

(element exceptionname
  (make element gi: "span"
	attributes: '(("class" "exceptionname"))
	(if (first-sibling?)
	    (literal "&#RE;          throws ")
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
    (make element gi: "code"
	  attributes: (list (list "class" (gi nd)))
	  (if (first-sibling?)
	      (literal "&#RE;")
	      (empty-sosofo))
	  (literal "  ")
	  (process-node-list modifiers)
	  (process-node-list type)
	  (process-node-list methodname)
	  (literal "(")
	  (process-node-list param)
	  (literal ")")
	  (process-node-list excep)
	  (literal ";&#RE;"))))

;; ===== C++ =========================================================

(mode cs-cpp-mode

(element classsynopsis
  (let* ((classes      (node-list-filter-by-gi (children (current-node))
					       (list (normalize "classname")
						     (normalize "modifier"))))
	 (classname    (let loop ((nl classes) (cn (empty-node-list)))
			 (if (node-list-empty? nl)
			     cn
			     (if (equal? (gi (node-list-first nl))
					 (normalize "classname"))
				 (node-list cn (node-list-first nl))
				 (loop (node-list-rest nl)
				       (node-list cn (node-list-first nl)))))))

	 (superclasses (let loop ((nl classes))
			 (if (node-list-empty? nl)
			     (empty-node-list)
			     (if (equal? (gi (node-list-first nl))
					 (normalize "classname"))
				 (node-list-rest nl)
				 (loop (node-list-rest nl)))))))
    (make element gi: "pre"
	  attributes: '(("class" "classsynopsis"))
	  (process-node-list classname)
	  (process-node-list superclasses)
	  (literal "{&#RE;")
	  (process-node-list
	   (node-list-filter-by-gi
	    (children (current-node))
	    (list (normalize "constructorsynopsis")
		  (normalize "destructorsynopsis")
		  (normalize "fieldsynopsis")
		  (normalize "methodsynopsis")
		  (normalize "classsynopsisinfo"))))
	  (literal "}"))))

(element classsynopsisinfo
  ($verbatim-display$ %indent-classsynopsisinfo-lines%
		      %number-classsynopsisinfo-lines%))

(element modifier
  (make element gi: "span"
	attributes: '(("class" "modifier"))
	(process-children)
	(literal " ")))

(element classname
  (if (first-sibling?)
      (make sequence
	(literal "class ")
	(make element gi: "span"
	      attributes: '(("class" "classname"))
	      (process-children))
	(if (last-sibling?)
	    (empty-sosofo)
	    (literal ": ")))
      (make sequence
	(make element gi: "span"
	      attributes: '(("class" "superclass"))
	      (process-children))
	(if (last-sibling?)
	    (literal " ")
	    (literal ", ")))))

(element fieldsynopsis
  (make element gi: "code"
	attributes: '(("class" "fieldsynopsis"))
	(literal "  ")
	(process-children)
	(literal ";&#RE;")))

(element type
  (make element gi: "span"
	attributes: '(("class" "type"))
	(process-children)
	(literal " ")))

(element varname
  (make element gi: "span"
	attributes: '(("class" "varname"))
	(process-children)))

(element initializer
  (make element gi: "span"
	attributes: '(("class" "initializer"))
	(literal " = ")
	(process-children)))

(element constructorsynopsis
  (cpp-method-synopsis))

(element destructorsynopsis
  (cpp-method-synopsis))

(element methodsynopsis
  (cpp-method-synopsis))

(element void
  (make element gi: "span"
	attributes: '(("class" "void"))
	(literal "void ")))

(element methodname
  (process-children))

(element methodparam
  (make element gi: "span"
	attributes: '(("class" "methodparam"))
	(if (first-sibling?)
	    (empty-sosofo)
	    (literal ", "))
	(process-children)))

(element parameter
  (make element gi: "span"
	attributes: '(("class" "parameter"))
	(process-children)))

(element exceptionname
  (make element gi: "span"
	attributes: '(("class" "exceptionname"))
	(if (first-sibling?)
	    (literal "&#RE;          throws ")
	    (literal ", "))
	(process-children)))
)

(define (cpp-method-synopsis #!optional (nd (current-node)))
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
    (make element gi: "code"
	  attributes: (list (list "class" (gi nd)))
	  (if (first-sibling?)
	      (literal "&#RE;")
	      (empty-sosofo))
	  (literal "  ")
	  (process-node-list modifiers)
	  (process-node-list type)
	  (process-node-list methodname)
	  (literal "(")
	  (process-node-list param)
	  (literal ")")
	  (process-node-list excep)
	  (literal ";&#RE;"))))

;; ===== Perl ======================================================== 

(mode cs-perl-mode

(element classsynopsis
  (let* ((modifiers    (select-elements (children (current-node))
					(normalize "modifier")))
	 (classes      (select-elements (children (current-node))
					(normalize "classname")))
	 (classname    (node-list-first classes))
	 (superclasses (node-list-rest classes)))
  (make element gi: "pre"
	attributes: '(("class" "classsynopsis"))
	(literal "package ")
	(process-node-list classname)
	(literal ";&#RE;")
	(if (node-list-empty? superclasses)
	    (empty-sosofo)
	    (make sequence
	      (literal "@ISA = (");
	      (process-node-list superclasses)
	      (literal ";&#RE;")))
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
      (make element gi: "span"
	    attributes: '(("class" "classname"))
	    (process-children))
      (make sequence
	(make element gi: "span"
	      attributes: '(("class" "superclass"))
	      (process-children))
	(if (last-sibling?)
	    (empty-sosofo)
	    (literal ", ")))))

(element fieldsynopsis
  (make element gi: "code"
	attributes: '(("class" "fieldsynopsis"))
	(literal "  ");
	(process-children)
	(literal ";&#RE;")))

(element type
  (make element gi: "span"
	attributes: '(("class" "type"))
	(process-children)
	(literal " ")))

(element varname
  (make element gi: "span"
	attributes: '(("class" "varname"))
	(process-children)))

(element initializer
  (make element gi: "span"
	attributes: '(("class" "initializer"))
	(literal " = ")
	(process-children)
	(literal " ")))

(element constructorsynopsis
  (perl-method-synopsis))

(element destructorsynopsis
  (perl-method-synopsis))

(element methodsynopsis
  (perl-method-synopsis))

(element void
  (empty-sosofo))

(element methodname
  (make element gi: "span"
	attributes: '(("class" "methodname"))
	(process-children)))

(element methodparam
  (make element gi: "span"
	attributes: '(("class" "methodparam"))
	(if (first-sibling?)
	    (empty-sosofo)
	    (literal ", "))
	(process-children)))

(element parameter
  (make element gi: "span"
	attributes: '(("class" "parameter"))
	(process-children)))

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
    (make element gi: "code"
	  attributes: (list (list "class" (gi nd)))
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
  (make element gi: "pre"
	attributes: '(("class" "classsynopsis"))
	(literal "interface ")
	(process-node-list modifiers)
	(process-node-list classname)
	(if (node-list-empty? superclasses)
	    (literal " ")
	    (make sequence
	      (literal " : ")
	      (process-node-list superclasses)))
	(literal " {&#RE;")
	(process-node-list
	 (node-list-filter-by-gi
	  (children (current-node))
	  (list (normalize "constructorsynopsis")
		(normalize "destructorsynopsis")
		(normalize "fieldsynopsis")
		(normalize "methodsynopsis")
		(normalize "classsynopsisinfo"))))
	(literal "}"))))

(element classsynopsisinfo
  ($verbatim-display$ %indent-classsynopsisinfo-lines%
		      %number-classsynopsisinfo-lines%))

(element modifier
  (make element gi: "span"
	attributes: '(("class" "modifier"))
    (process-children)
    (literal " ")))

(element classname
  (if (first-sibling?)
      (make element gi: "span"
	    attributes: '(("class" "classname"))
	    (process-children))
      (make sequence
	(make element gi: "span"
	      attributes: '(("class" "superclass"))
	      (process-children))
	(if (last-sibling?)
	    (empty-sosofo)
	    (literal ", ")))))

(element fieldsynopsis
  (make element gi: "code"
	attributes: '(("class" "fieldsynopsis"))
	(literal "  ");
	(process-children)
	(literal ";&#RE;")))

(element type
  (make element gi: "span"
	attributes: '(("class" "type"))
	(process-children)
	(literal " ")))

(element varname
  (make element gi: "span"
	attributes: '(("class" "varname"))
	(process-children)))

(element initializer
  (make element gi: "span"
	attributes: '(("class" "initializer"))
	(literal " = ")
	(process-children)
	(literal " ")))

(element constructorsynopsis
  (idl-method-synopsis))

(element destructorsynopsis
  (idl-method-synopsis))

(element methodsynopsis
  (idl-method-synopsis))

(element void
  (make element gi: "span"
	attributes: '(("class" "void"))
	(literal "void ")))

(element methodname
  (make element gi: "span"
	attributes: '(("class" "methodname"))
	(process-children)))

(element methodparam
  (make element gi: "span"
	attributes: '(("class" "methodparam"))
	(if (first-sibling?)
	    (empty-sosofo)
	    (literal ", "))
	(process-children)))

(element parameter
  (make element gi: "span"
	attributes: '(("class" "parameter"))
	(process-children)))

(element exceptionname
  (make element gi: "span"
	attributes: '(("class" "exceptionname"))
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
    (make element gi: "code"
	  attributes: (list (list "class" (gi nd)))
	  (literal "  ")
	  (process-node-list modifiers)
	  (process-node-list type)
	  (process-node-list methodname)
	  (literal "(")
	  (process-node-list param)
	  (literal ")")
	  (process-node-list excep)
	  (literal ";&#RE;"))))

;; ===== Python ======================================================= 
;; Contributed by Lane Stevens, lane@cycletime.com

(mode cs-python-mode
  (element classsynopsis
    (let* ((classes      (select-elements (children (current-node))
					  (normalize "ooclass")))
	   (classname    (node-list-first classes))
	   (superclasses (node-list-rest classes)))
      (make element gi: "pre"
	    attributes: '(("class" "classsynopsis"))
	    (literal "class ")
	    (process-node-list classname)
	    (literal "(")
	    (process-node-list superclasses)
	    (literal ") :")
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
      (make element gi: "SPAN"
	    attributes: '(("class" "ooclass"))
	    (process-children)
	    (cond
	     ((first-sibling?) (literal " "))
	     ((last-sibling?) (empty-sosofo))
	     (#t (literal ", "))
	     )
	    )
      )
    )
	    
  (element classname
    (if (first-sibling?)
	(make element gi: "SPAN"
	      attributes: '(("class" "classname"))
	      (process-children))
	(make element gi: "SPAN"
	      attributes: '(("class" "superclass")))
	)
    )

  (element methodsynopsis
    (python-method-synopsis))

  (element initializer
    (make element gi: "SPAN"
	  attributes: '(("class" "initializer"))
	  (literal " = ")
	  (process-children)))
  
  (element methodname
    (process-children))

  (element methodparam
    (make element gi: "SPAN"
	  attributes: '(("class" "methodparam"))
	  (process-children)
	  (if (last-sibling?)
	      (empty-sosofo)
	      (literal ", "))
	  )
    )
	      
    
  (element parameter
    (make element gi: "SPAN"
	  attributes: '(("class" "parameter"))
	  (process-children)))

  
  )

(define (python-method-synopsis #!optional (nd (current-node)))
  (let* ((the-method-name (select-elements (children nd) (normalize "methodname")))
	 (the-method-params (select-elements (children nd) (normalize "methodparam"))))
    (make element gi: "code"
	  attributes: (list (list "class" (gi nd)))
	  (literal "    def ")
	  (process-node-list the-method-name)
	  (literal "(")
	  (process-node-list the-method-params)
	  (literal ") :")
	  )
    )
  )

;; EOF
