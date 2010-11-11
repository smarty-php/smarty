;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================= GLOSSARY ELEMENTS ==========================

;; HACK ALERT!  There is no top-level wrapper around one or more GLOSSENTRYs,
;; so this code has to look around and output the right thing for the
;; front matter and then the GLOSSENTRYs.  Ugh.

(define ($glossary-frontmatter$)
  (let loop ((nl (children (current-node))) (headlist (empty-node-list)))
    (if (node-list-empty? nl)
	headlist
	(if (equal? (gi (node-list-first nl)) (normalize "glossentry"))
	    headlist
	    (loop (node-list-rest nl) (node-list
				       headlist
				       (node-list-first nl)))))))

(define ($glossary-glossentrys$)
  (let loop ((nl (children (current-node))) (gelist (empty-node-list)))
    (if (node-list-empty? nl)
	gelist
	(loop (node-list-rest nl)
	      (if (equal? (gi (node-list-first nl)) (normalize "glossentry"))
		  (node-list gelist (node-list-first nl))
		  gelist)))))

(define ($glossary-body$)
  (make element gi: "DIV"
	attributes: '(("CLASS" "GLOSSARY"))
	($component-title$)
	(process-node-list ($glossary-frontmatter$))
	(if (not (node-list-empty? ($glossary-glossentrys$)))
	    (make element gi: "DL"
		  (process-node-list ($glossary-glossentrys$)))
	    (empty-sosofo))))

(element glossary
  (html-document (with-mode head-title-mode 
		   (literal (element-title-string (current-node))))
		 ($glossary-body$)))

(element (glossary title) (empty-sosofo))

(element glossdiv 
  (make element gi: "DIV"
	attributes: (list 
		     (list "CLASS" (gi)))
	($section-title$)
	(process-node-list ($glossary-frontmatter$))
	(if (not (node-list-empty? ($glossary-glossentrys$)))
	    (make element gi: "DL"
		  (process-node-list ($glossary-glossentrys$)))
	    (empty-sosofo))))

(element (glossdiv title) (empty-sosofo))

(element glosslist 
  (make element gi: "DIV"
	attributes: (list 
		     (list "CLASS" (gi)))
	(make element gi: "DL"
	      (process-children))))

(element glossentry (process-children))

(element (glossentry glossterm) 
  (let ((id (attribute-string (normalize "id") (parent (current-node)))))
    (make element gi: "DT"
	  (if id
	      (make sequence
		(make element gi: "A"
		      attributes: (list
				   (list "NAME" id))
		      (empty-sosofo))
		    (make element gi: "B"
			  (process-children)))
	      (make element gi: "B"
		    (process-children))))))

(element (glossentry acronym)
  (make sequence
    (literal " (")
    (process-children)
    (literal ")")))

(element (glossentry abbrev) (empty-sosofo))

(element (glossentry glossdef)
  (make element gi: "DD"
	(process-children)))

(element (glossterm revhistory)
  (empty-sosofo))

(element (glossentry glosssee)
  (make element gi: "DD"
	(if (attribute-string (normalize "otherterm"))
	    (make element gi: "P"
	      (make element gi: "EM"
		    (literal (gentext-element-name (gi))
			     (gentext-label-title-sep (gi))))
	      (make element gi: "A"
		    attributes: (list (list "HREF"
					    (link-target 
					     (attribute-string
					      (normalize "otherterm")))))
		    (with-mode otherterm
		      (process-element-with-id
		       (attribute-string (normalize "otherterm"))))))
	    (process-children))))

;; When we hit the first GLOSSSEEALSO, process all of them as a node-list
(element glossseealso
  (if (first-sibling?)
      (make element gi: "P"
	    (make sequence
	      (make element gi: "EM"
		    (literal (gentext-element-name (gi))
			     (gentext-label-title-sep (gi))))
	      (with-mode glossseealso
		(process-node-list
		 (select-elements (children (parent)) '(glossseealso))))
	      (literal ".")))
      (empty-sosofo)))

(mode glossseealso
  (element glossseealso
    (make sequence
      (if (first-sibling?)
	  (empty-sosofo)
	  (make element gi: "EM"
		(literal ", ")))
      (if (attribute-string (normalize "otherterm")) ;; but this should be required...
	  (make element gi: "A"
		attributes: (list (list "HREF"
					(link-target
					 (attribute-string
					  (normalize "otherterm")))))
		(with-mode otherterm
		  (process-element-with-id
		   (attribute-string (normalize "otherterm")))))
	  (process-children)))))

;; This is referenced within the GLOSSSEE and GLOSSSEEALSO element
;; construction expressions.  The OTHERTERM attributes on GLOSSSEE and
;; GLOSSSEEALSO (should) refer to GLOSSENTRY elements but we're only
;; interested in the text within the GLOSSTERM.  Discard the revision
;; history and the definition from the referenced term.
(mode otherterm
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
  (let* ((linkend (attribute-string (normalize "linkend"))))
    (if linkend
	(make element gi: "A"
	      attributes: (list (list "HREF" (href-to (element-with-id 
						       linkend))))
	      ($italic-seq$))
	($italic-seq$))))

;; a first glossterm
(element firstterm
  (let* ((linkend (attribute-string (normalize "linkend")))
	 (sosofo  (if linkend
		      (make element gi: "A"
			    attributes: (list (list "HREF" 
						    (href-to 
						     (element-with-id 
						      linkend))))
			    ($italic-seq$))
		      ($italic-seq$))))
    (if firstterm-bold
	(make element gi: "B"
	      sosofo)
	sosofo)))

