;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; This module implements support for elements introduced in DocBook 3.1.
;; When DocBook 3.1 is officially released, these rules will get folded
;; into more appropriate modules.

;; ======================================================================
;; MediaObject and friends...

(define preferred-mediaobject-notations
  (list "JPG" "JPEG" "PNG" "linespecific"))

(define preferred-mediaobject-extensions
  (list "jpeg" "jpg" "png" "avi" "mpg" "mpeg" "qt"))

(define acceptable-mediaobject-notations
  (list "GIF" "GIF87a" "GIF89a" "BMP" "WMF"))

(define acceptable-mediaobject-extensions
  (list "gif" "bmp" "wmf"))

(element mediaobject
  (make element gi: "DIV"
	attributes: (list (list "CLASS" (gi)))
	(make element gi: "P"
	      ($mediaobject$))))

(element inlinemediaobject
  (make element gi: "SPAN"
	attributes: (list (list "CLASS" (gi)))
	($mediaobject$)))

(element mediaobjectco
  (process-children))

(element imageobjectco
  (process-children))

(element objectinfo
  (empty-sosofo))

(element videoobject
  (process-children))

(element videodata
  (let ((filename (data-filename (current-node))))
    (make element gi: "EMBED"
	  attributes: (list (list "SRC" filename)))))

(element audioobject
  (process-children))

(element audiodata
  (let ((filename (data-filename (current-node))))
    (make element gi: "EMBED"
	  attributes: (list (list "SRC" filename)))))

(element imageobject
  (process-children))

(element imagedata
  (let* ((filename (data-filename (current-node)))
	 (mediaobj (parent (parent (current-node))))
	 (textobjs (select-elements (children mediaobj) 
				    (normalize "textobject")))
	 (alttext  (let loop ((nl textobjs) (alttext #f))
		     (if (or alttext (node-list-empty? nl))
			 alttext
			 (let ((phrase (select-elements 
					(children 
					 (node-list-first nl))
					(normalize "phrase"))))
			   (if (node-list-empty? phrase)
			       (loop (node-list-rest nl) #f)
			       (loop (node-list-rest nl)
				     (data (node-list-first phrase))))))))
	 (fileref   (attribute-string (normalize "fileref")))
	 (entityref (attribute-string (normalize "entityref")))
	 (format    (if (attribute-string (normalize "format"))
			(attribute-string (normalize "format"))
			(if entityref
			    (entity-notation entityref)
			    #f))))
    (if (equal? format (normalize "linespecific"))
	(if fileref
	    (include-file fileref)
	    (include-file (entity-generated-system-id entityref)))
	($img$ (current-node) alttext))))

(element textobject
  (make element gi: "DIV"
	attributes: (list (list "CLASS" (gi)))
	(process-children)))

(element caption
  (make element gi: "DIV"
	attributes: (list (list "CLASS" (gi)))
	(process-children)))

;; ======================================================================
;; InformalFigure

(element informalfigure
  ($informal-object$ %informalfigure-rules% %informalfigure-rules%))

;; ======================================================================
;; Colophon

(element colophon
  ($component$))

;; ======================================================================
;; section
;; sectioninfo

(element section ($section$))
(element (section title) (empty-sosofo))

;; ======================================================================
;; QandASet and friends

(define (qanda-defaultlabel)
  (normalize "number"))

(define (qanda-section-level)
  ;; FIXME: what if they nest inside each other?
  (let* ((enclsect (ancestor-member (current-node)
				    (list (normalize "section")
					  (normalize "simplesect")
					  (normalize "sect5")
					  (normalize "sect4")
					  (normalize "sect3")
					  (normalize "sect2")
					  (normalize "sect1")
					  (normalize "refsect3")
					  (normalize "refsect2")
					  (normalize "refsect1")))))
    (SECTLEVEL enclsect)))

(define (qandadiv-section-level)
  (let ((depth (length (hierarchical-number-recursive 
			(normalize "qandadiv")))))
    (+ (qanda-section-level) depth)))

(element qandaset
  (let ((title (select-elements (children (current-node)) 
				(normalize "title")))
	;; process title and rest separately so that we can put the TOC
	;; in the rigth place...
	(rest  (node-list-filter-by-not-gi (children (current-node))
					   (list (normalize "title")))))
    (make element gi: "DIV"
	  attributes: (list (list "CLASS" (gi)))
	  (process-node-list title)
	  (if ($generate-qandaset-toc$)
	      (process-qanda-toc)
	      (empty-sosofo))
	  (process-node-list rest))))

(element (qandaset title)
  (let* ((htmlgi  (string-append "H" (number->string 
				      (+ (qanda-section-level) 1)))))
    (make element gi: htmlgi
	  attributes: (list (list "CLASS" (gi (current-node))))
	  (process-children))))

(element qandadiv
  (make element gi: "DIV"
	attributes: (list (list "CLASS" (gi)))
	(process-children)))

(element (qandadiv title)
  (let* ((hnr     (hierarchical-number-recursive (normalize "qandadiv")
						 (current-node)))
	 (number  (let loop ((numlist hnr) (number "") (sep ""))
		    (if (null? numlist)
			number
			(loop (cdr numlist) 
			      (string-append number
					     sep
					     (number->string (car numlist)))
			      "."))))
	 (htmlgi  (string-append "H" (number->string 
				      (+ (qandadiv-section-level) 1)))))
    (make element gi: htmlgi
	  (make element gi: "A"
		attributes: (list (list "NAME" (element-id 
						(parent (current-node)))))
		(empty-sosofo))
	  (literal number ". ")
	  (process-children))))

(element qandaentry
  (make element gi: "DIV"
	attributes: (list (list "CLASS" (gi)))
	(process-children)))

(element question
  (let* ((chlist   (children (current-node)))
	 (firstch  (node-list-first chlist))
	 (restch   (node-list-rest chlist)))
    (make element gi: "DIV"
	  attributes: (list (list "CLASS" (gi)))
	  (make element gi: "P"
		(make element gi: "A"
		      attributes: (list (list "NAME" (element-id)))
		      (empty-sosofo))
		(make element gi: "B"
		      (literal (question-answer-label (current-node)) " "))
		(process-node-list (children firstch)))
	  (process-node-list restch))))

(element answer
  (let* ((inhlabel (inherited-attribute-string (normalize "defaultlabel")))
	 (deflabel (if inhlabel inhlabel (qanda-defaultlabel)))
	 (label    (attribute-string (normalize "label")))
	 (chlist   (children (current-node)))
	 (firstch  (node-list-first chlist))
	 (restch   (node-list-rest chlist)))
    (make element gi: "DIV"
	  attributes: (list (list "CLASS" (gi)))
	  (make element gi: "P"
		(make element gi: "B"
		      (literal (question-answer-label (current-node)) " "))
		(process-node-list (children firstch)))
	  (process-node-list restch))))

;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

(define (process-qanda-toc #!optional (node (current-node)))
  (let* ((divs     (node-list-filter-by-gi (children node)
					   (list (normalize "qandadiv"))))
	 (entries  (node-list-filter-by-gi (children node)
					   (list (normalize "qandaentry"))))
	 (inhlabel (inherited-attribute-string (normalize "defaultlabel")))
	 (deflabel (if inhlabel inhlabel (qanda-defaultlabel))))
    (make element gi: "DL"
	  (with-mode qandatoc
	    (process-node-list divs))
	  (with-mode qandatoc
	    (process-node-list entries)))))

(mode qandatoc
  (element qandadiv
    (let ((title (select-elements (children (current-node))
				  (normalize "title"))))
      (make sequence
	(make element gi: "DT"
	      (process-node-list title))
	(make element gi: "DD"
	      (process-qanda-toc)))))
  
  (element (qandadiv title)
    (let* ((hnr     (hierarchical-number-recursive (normalize "qandadiv")
						   (current-node)))
	   (number  (let loop ((numlist hnr) (number "") (sep ""))
		      (if (null? numlist)
			  number
			  (loop (cdr numlist) 
				(string-append number
					       sep
					       (number->string (car numlist)))
				".")))))
      (make sequence
	(literal number ". ")
	(make element gi: "A"
	      attributes: (list (list "HREF" 
				      (href-to (parent (current-node)))))
	      (process-children)))))

  (element qandaentry
    (process-children))

  (element question
    (let* ((chlist   (children (current-node)))
	   (firstch  (node-list-first chlist)))
      (make element gi: "DT"
	    (literal (question-answer-label (current-node)) " ")
	    (make element gi: "A"
		  attributes: (list (list "HREF" (href-to (current-node))))
		  (process-node-list (children firstch))))))
  
  (element answer
    (empty-sosofo))
)

;; ======================================================================
;; constant

(element constant 
  ($mono-seq$))

;; ======================================================================
;; varname

(element varname
  ($mono-seq$))
