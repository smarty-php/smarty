;; -*- Scheme -*-
;;
;; $Id$
;;
;; This file contains stylesheet customization common to the HTML
;; and print versions.
;;

;; Stylesheets Localization
(define %default-language% "en")

(define %root-filename% "index")
(define %use-id-as-filename% #t)
(define %gentext-nav-tblwidth% "100%")
(define %refentry-function% #t)
(define %refentry-generate-name% #f)
(define %funcsynopsis-style% 'ansi)
(define ($legalnotice-link-file$ legalnotice)
  (string-append "copyright" %html-ext%))
(define %generate-legalnotice-link% #t)
(define %footnotes-at-end% #t)
(define %force-chapter-toc% #t)
(define newline "\U-000D")
(define %number-programlisting-lines% #f)
(define %linenumber-mod% 1)
(define %shade-verbatim% #t)
(define %prefers-ordinal-label-name-format% #f)
(define ($generate-book-lot-list$) (list))

(define (php-code code)
  (make processing-instruction
    data: (string-append "php " code "?")))

(define quicksort
  (quicksort::generic null? car cdr append cons '()))

(define nl-quicksort
  (quicksort::generic node-list-empty?
		      node-list-first
		      node-list-rest
		      node-list
		      node-list
		      (empty-node-list)))

(define quicksort::generic
  (lambda(is-null? first others concat add empty)
    (letrec ((collect
	;; Collect is an helper function doing the real work

	      (lambda (pivot ls lgroup rgroup less?)
		(if (is-null? ls)
		    (concat (impl lgroup less?)
			    (add pivot (impl rgroup less?)))
		    (if (less? pivot (first ls))
			(collect pivot (others ls) lgroup 
				 (add (first ls) rgroup) 
				 less?)
			(collect pivot (others ls) 
				 (add (first ls) lgroup) 
				 rgroup 
				 less?)))))
	     (impl	
	      ;; impl first test some trivial sorting case and then call
	      ;; the procedure collect
	      (lambda (ls less?)	
		(if (or (is-null? ls) (is-null? (others ls)))
		    ls
		    (collect (first ls) (others ls) empty empty less?)))))
	;; we return the new defined procedure
      impl)))

;; Polish definitions

(define (gentext-pl-nav-next next)
  (make sequence (literal "Nast\U-0119;pny")))

(define (book-titlepage-recto-elements)
  (list (normalize "title")
	(normalize "subtitle")
	(normalize "graphic")
	(normalize "mediaobject")
	(normalize "corpauthor")
	(normalize "authorgroup")
	(normalize "author")
	(normalize "editor")
	(normalize "pubdate")
	(normalize "copyright")
	(normalize "abstract")
	(normalize "legalnotice")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; render function prototypes, esp. with optional arguments
;;;; for new docbook4 methodsynopsis tag and friends
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helper function generating closing optional brackets
(define (methodsynopsis-generate-closing-optionals nl)
  (if (node-list-empty? nl)
      (empty-sosofo) ;; empty list -> do nothing
      (make sequence ;; process list node
        (if (or (and (attribute-string (normalize "choice") (node-list-first nl))
										 (string=? "opt" (attribute-string (normalize "choice") (node-list-first nl)))
										 )
								(has-default-value (node-list-first nl)))
                (literal %arg-choice-opt-close-str%) ;; is optional parameter -> close a bracket
                (empty-sosofo)
                )         
        (methodsynopsis-generate-closing-optionals (node-list-rest nl)) ;; process rest of list
        )
      )
  )

(define (is-true-optional nl)
  (and (equal? (gi (parent nl)) (normalize "parameter"))
       (equal? 0 (string-length (strip (data (preced nl)))))
       (equal? 0 (string-length (strip (data (follow nl)))))
       )
  )


(define (has-true-optional nl)
  (is-true-optional 
   (node-list-first-element 
    (select-elements 
     (descendants nl) 
     (normalize "optional"))
    )
   )
  )


(define (count-true-optionals nl)
  (let loop 
      ((result 0)
       (nl (select-elements (descendants nl) (normalize "optional")))
       )
    (if(node-list-empty? nl)
       result
       (if(is-true-optional(node-list-first nl))
          (loop (+ result 1) (node-list-rest nl))
          (loop result (node-list-rest nl))
          )
       )
    )
  )

(define (has-default-value nl)
	(not (node-list-empty? ( select-elements (descendants nl) (normalize "initializer"))))
	)

;; render classsynopsis
(element classsynopsis 
  (make sequence 
    (literal "class ")
    (process-children-trim)
    (literal "}")
  )
)

(element ooclass (make sequence (process-children)))

(element (ooclass classname)
  (make sequence
	($bold-seq$
	 (process-children-trim)
	 )
	(literal " { ")
	(linebreak)
	)
  )



;; render methodsynopsis
(element methodsynopsis 
	(make sequence 
    (process-children) ;; render content
    (methodsynopsis-generate-closing-optionals (children (current-node))) ;; close optional brackets
    (literal ")") ;; close parameter list
		(linebreak)
    )
)

(element constructorsynopsis 
	(make sequence 
    (process-children) ;; render content
    (methodsynopsis-generate-closing-optionals (children (current-node))) ;; close optional brackets
    (literal ")") ;; close parameter list
		(linebreak)
    )
)

;; render return type
(element (methodsynopsis type)
	(make sequence 
		(process-children-trim)
		(literal " ")
		)
)

;; render function name
(element (methodsynopsis methodname)
	(make sequence
		($bold-seq$
		 (process-children-trim)
		 )
		(literal " ( ") ;; start parameter list
		)
	)

(element (constructorsynopsis methodname)
	(make sequence
		($bold-seq$
		 (process-children-trim)
		 )
		(literal " ( ") ;; start parameter list
		)
	)

;; render parameters
(element (methodsynopsis methodparam)
	(make sequence 
    ;; special case -> first parameter is optional
    (if (equal? (gi (ipreced (current-node))) (normalize "methodparam"))
        (empty-sosofo) ;; have prev. parameters -> is not first
        (if  (or (equal? (attribute-string (normalize "choice")) "opt")
								 (has-default-value (current-node))
								 )
						 (literal %arg-choice-opt-open-str%) ;; generate opening bracket
             (empty-sosofo) 
             )
        )

		(process-children-trim)

    ;; have more parameters following me?
    (if (equal? (gi (ifollow (current-node))) (normalize "methodparam"))
        (make sequence
          ;; is next parameter optional?
          (if  (or (equal? (attribute-string (normalize "choice") (ifollow (current-node))) "opt")
									 (has-default-value (ifollow (current-node)))
									 )
               (make sequence 
                 (literal " ")
                 (literal %arg-choice-opt-open-str%)
                 )
               (empty-sosofo)
               )
          ;; parameter list separator
          (literal ", ")
          )
        (empty-sosofo)
        )
    )
  )


;; render parameters
(element (constructorsynopsis methodparam)
	(make sequence 
    ;; special case -> first parameter is optional
    (if (equal? (gi (ipreced (current-node))) (normalize "methodparam"))
        (empty-sosofo) ;; have prev. parameters -> is not first
        (if  (or (equal? (attribute-string (normalize "choice")) "opt")
								 (has-default-value (current-node))
								 )
						 (literal %arg-choice-opt-open-str%) ;; generate opening bracket
             (empty-sosofo) 
             )
        )

		(process-children-trim)

    ;; have more parameters following me?
    (if (equal? (gi (ifollow (current-node))) (normalize "methodparam"))
        (make sequence
          ;; is next parameter optional?
          (if  (or (equal? (attribute-string (normalize "choice") (ifollow (current-node))) "opt")
									 (has-default-value (ifollow (current-node)))
									 )
               (make sequence 
                 (literal " ")
                 (literal %arg-choice-opt-open-str%)
                 )
               (empty-sosofo)
               )
          ;; parameter list separator
          (literal ", ")
          )
        (empty-sosofo)
        )
    )
  )

;; special "void" return type tag
(element (methodsynopsis void)
	(literal "void ")
)

  
;; render parameter type
(element (methodparam type)
	(make sequence 
		(process-children-trim)
		(literal " ")
		)
	)

;; render parameter name
(element (methodparam parameter)
	(make sequence
		(process-children-trim)
		)
	)

;; render default value
(element (methodparam initializer)
	(make sequence 
		(literal "=")
    ($italic-seq$ (process-children-trim))
    )
  )                                                       


;; render fieldsynopsis
(element fieldsynopsis
  (make sequence
	(process-children)
	(linebreak)
	)
)

(element (fieldsynopsis type)
	(make sequence 
		(process-children-trim)
		(literal " ")
		)
)
(element (fieldsynopsis varname)
	(make sequence 
		(process-children-trim)
	)
)




;; render SGML tags
(element sgmltag
  (make sequence
    ($bold-seq$ (literal "<"))
    ($bold-seq$ (process-children))
    ($bold-seq$ (literal ">"))
  )
)

