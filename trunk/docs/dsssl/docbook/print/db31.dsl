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
  (list "EPS" "PS" "JPG" "JPEG" "PNG" "linespecific"))

(define preferred-mediaobject-extensions
  (list "eps" "ps" "jpg" "jpeg" "png"))

(define acceptable-mediaobject-notations
  (list "GIF" "GIF87a" "GIF89a" "BMP" "WMF"))

(define acceptable-mediaobject-extensions
  (list "gif" "bmp" "wmf"))

(element mediaobject
  (make paragraph
    ($mediaobject$)))

(element inlinemediaobject
  (make sequence
    ($mediaobject$)))

(element mediaobjectco
  (error "MediaObjectCO is not supported yet."))

(element imageobjectco
  (error "ImageObjectCO is not supported yet."))

(element objectinfo
  (empty-sosofo))

(element videoobject
  (process-children))

(element videodata
  (empty-sosofo))

(element audioobject
  (process-children))

(element audiodata
  (empty-sosofo))

(element imageobject
  (process-children))

(element imagedata
  (if (have-ancestor? (normalize "mediaobject"))
      ($img$ (current-node) #t)
      ($img$ (current-node) #f)))

(element textobject
  (make display-group
    (process-children)))

(element caption
  (process-children))

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

(element qandaset
  (let ((title (select-elements (children (current-node)) 
				(normalize "title"))))
    (make display-group
      (process-node-list title)
      (process-qanda))))

(element (qandaset title)
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
					  (normalize "refsect1"))))
	 (sectlvl (SECTLEVEL enclsect))
	 (hs      (HSIZE (- 4 (+ sectlvl 1)))))
    (make paragraph
      font-family-name: %title-font-family%
      font-weight:  (if (< sectlvl 5) 'bold 'medium)
      font-posture: (if (< sectlvl 5) 'upright 'italic)
      font-size: hs
      line-spacing: (* hs %line-spacing-factor%)
      space-before: (* hs %head-before-factor%)
      space-after: (* hs %head-after-factor%)
      start-indent: %body-start-indent%
      first-line-start-indent: 0pt
      quadding: %section-title-quadding%
      keep-with-next?: #t
      (process-children))))

(element qandadiv
  (let ((title (select-elements (children (current-node)) 
				(normalize "title"))))
    (make sequence
      (process-node-list title)
      (make display-group
	start-indent: (+ (inherited-start-indent) 2pi)
	(process-qanda)))))

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
    (make paragraph
      font-weight: 'bold
      space-after: %block-sep%
      (literal number ". ")
      (process-children))))

(define (process-qanda #!optional (node (current-node)))
  (let* ((preamble (node-list-filter-by-not-gi 
		    (children node)
		    (list (normalize "title")
			  (normalize "qandadiv") 
			  (normalize "qandaentry"))))
	 (divs     (node-list-filter-by-gi (children node)
					   (list (normalize "qandadiv"))))
	 (entries  (node-list-filter-by-gi (children node)
					   (list (normalize "qandaentry"))))
	 (inhlabel (inherited-attribute-string (normalize "defaultlabel")))
	 (deflabel (if inhlabel inhlabel (qanda-defaultlabel))))
    (make sequence
      (process-node-list preamble)
      (process-node-list divs)
      (process-node-list entries))))

(element qandaentry
  (process-children))

;; space-after on quanda answer is excessive; keep with next should be
;; upstream
;; Adam Di Carlo, adam@onshore.com
(element question
  (let* ((chlist   (children (current-node)))
         (firstch  (node-list-first chlist))
         (restch   (node-list-rest chlist))
	 (label    (question-answer-label (current-node))))
    (make sequence
      (make paragraph
	space-after: (/ %para-sep% 2)
	keep-with-next?: #t
	(make sequence
	  (make sequence
	    font-weight: 'bold
	    (if (string=? label "")
		(empty-sosofo)
		(literal label " ")))
	  (process-node-list (children firstch)))
      (process-node-list restch)))))

(element answer
  (let* ((chlist   (children (current-node)))
	 (firstch  (node-list-first chlist))
	 (restch   (node-list-rest chlist))
	 (label    (question-answer-label (current-node))))
    (make display-group
      space-after: %block-sep%
      (make paragraph
	(make sequence
	  (make sequence
	    font-weight: 'bold
	    (if (string=? label "")
		(empty-sosofo)
		(literal label " ")))
	  (process-node-list (children firstch))))
      (process-node-list restch))))

;; ======================================================================
;; constant

(element constant 
  ($mono-seq$))

;; ======================================================================
;; varname

(element varname
  ($mono-seq$))
