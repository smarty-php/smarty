;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

(define (have-sibling? sibling-gi #!optional (node (current-node)))
  (let loop ((nl (children (parent node))))
    (if (node-list-empty? nl) 
	#f
	(if (equal? (gi (node-list-first nl)) sibling-gi)
	    #t
	    (loop (node-list-rest nl))))))

(define (titlepage-content? elements gis)
  (let giloop ((gilist gis))
    (if (null? gilist)
	#f
	(if (not (node-list-empty? (node-list-filter-by-gi 
				    elements 
				    (list (car gilist)))))
	    #t
	    (giloop (cdr gilist))))))

(define (titlepage-gi-list-by-elements elements nodelist) 
  ;; Elements is a list of GIs.  Nodelist is a list of nodes.
  ;; This function returns all of the nodes in nodelist that
  ;; are in elements in the order they occur in elements.
  (let loop ((gilist elements) (rlist (empty-node-list)))
    (if (null? gilist)
	rlist
	(loop (cdr gilist) 
	      (node-list rlist (node-list-filter-by-gi 
				nodelist (list (car gilist))))))))

(define (titlepage-gi-list-by-nodelist elements nodelist) 
  ;; Elements is a list of GIs.  Nodelist is a list of nodes.
  ;; This function returns all of the nodes in nodelist that
  ;; are in elements in the order they occur in nodelist.
  (let loop ((nl nodelist) (rlist (empty-node-list)))
    (if (node-list-empty? nl)
	rlist
	(if (member (gi (node-list-first nl)) elements)
	    (loop (node-list-rest nl)
		  (node-list rlist (node-list-first nl)))
	    (loop (node-list-rest nl) rlist)))))

(define (titlepage-nodelist elements nodelist)
  ;; We expand BOOKBIBLIO, BIBLIOMISC, and BIBLIOSET in the element
  ;; list because that level of wrapper usually isn't significant.
  (let ((exp-nodelist (expand-children nodelist (list (normalize "bookbiblio") 
						      (normalize "bibliomisc")
						      (normalize "biblioset")))))
    (if %titlepage-in-info-order%
	(titlepage-gi-list-by-nodelist elements exp-nodelist)
	(titlepage-gi-list-by-elements elements exp-nodelist))))

(mode titlepage-address-mode
  (default (process-children)))

;; == Title pages for SETs ==============================================

(define (set-titlepage-recto-elements)
  (list (normalize "title") 
	(normalize "subtitle") 
	(normalize "graphic") 
	(normalize "mediaobject")
	(normalize "corpauthor") 
	(normalize "authorgroup") 
	(normalize "author") 
	(normalize "editor")))

(define (set-titlepage-verso-elements)
  (list (normalize "title") 
	(normalize "subtitle") 
	(normalize "corpauthor") 
	(normalize "authorgroup") 
	(normalize "author") 
	(normalize "editor")
	(normalize "edition") 
	(normalize "pubdate") 
	(normalize "copyright")
	(normalize "legalnotice") 
	(normalize "revhistory")))

(define (set-titlepage-content? elements side)
  (titlepage-content? elements (if (equal? side 'recto)
				   (set-titlepage-recto-elements)
				   (set-titlepage-verso-elements))))

(define (set-titlepage elements #!optional (side 'recto))
  (let ((nodelist (titlepage-nodelist 
		   (if (equal? side 'recto)
		       (set-titlepage-recto-elements)
		       (set-titlepage-verso-elements))
		   elements)))
;;    (make simple-page-sequence
;;      page-n-columns: %titlepage-n-columns%
;;      input-whitespace-treatment: 'collapse
;;      use: default-text-style
      (make sequence

      ;; This hack is required for the RTF backend. If an external-graphic
      ;; is the first thing on the page, RTF doesn't seem to do the right
      ;; thing (the graphic winds up on the baseline of the first line
      ;; of the page, left justified).  This "one point rule" fixes
      ;; that problem.
      (make paragraph
	line-spacing: 1pt
	(literal ""))

      (let loop ((nl nodelist) (lastnode (empty-node-list)))
	(if (node-list-empty? nl)
	    (empty-sosofo)
	    (make sequence
	      (if (or (node-list-empty? lastnode)
		      (not (equal? (gi (node-list-first nl))
				   (gi lastnode))))
		  (set-titlepage-before (node-list-first nl) side)
		  (empty-sosofo))
	      (cond
		((equal? (gi (node-list-first nl)) (normalize "abbrev"))
		 (set-titlepage-abbrev (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "abstract"))
		 (set-titlepage-abstract (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "address"))
		 (set-titlepage-address (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "affiliation"))
		 (set-titlepage-affiliation (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "artpagenums"))
		 (set-titlepage-artpagenums (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "author"))
		 (set-titlepage-author (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "authorblurb"))
		 (set-titlepage-authorblurb (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "authorgroup"))
		 (set-titlepage-authorgroup (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "authorinitials"))
		 (set-titlepage-authorinitials (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "bibliomisc"))
		 (set-titlepage-bibliomisc (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "biblioset"))
		 (set-titlepage-biblioset (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "bookbiblio"))
		 (set-titlepage-bookbiblio (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "citetitle"))
		 (set-titlepage-citetitle (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "collab"))
		 (set-titlepage-collab (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "confgroup"))
		 (set-titlepage-confgroup (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "contractnum"))
		 (set-titlepage-contractnum (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "contractsponsor"))
		 (set-titlepage-contractsponsor (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "contrib"))
		 (set-titlepage-contrib (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "copyright"))
		 (set-titlepage-copyright (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "corpauthor"))
		 (set-titlepage-corpauthor (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "corpname"))
		 (set-titlepage-corpname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "date"))
		 (set-titlepage-date (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "edition"))
		 (set-titlepage-edition (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "editor"))
		 (set-titlepage-editor (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "firstname"))
		 (set-titlepage-firstname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "graphic"))
		 (set-titlepage-graphic (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "honorific"))
		 (set-titlepage-honorific (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "indexterm"))
		 (set-titlepage-indexterm (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "invpartnumber"))
		 (set-titlepage-invpartnumber (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "isbn"))
		 (set-titlepage-isbn (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "issn"))
		 (set-titlepage-issn (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "issuenum"))
		 (set-titlepage-issuenum (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "itermset"))
		 (set-titlepage-itermset (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "keywordset"))
		 (set-titlepage-keywordset (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "legalnotice"))
		 (set-titlepage-legalnotice (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "lineage"))
		 (set-titlepage-lineage (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "mediaobject"))
		 (set-titlepage-mediaobject (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "modespec"))
		 (set-titlepage-modespec (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "orgname"))
		 (set-titlepage-orgname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "othercredit"))
		 (set-titlepage-othercredit (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "othername"))
		 (set-titlepage-othername (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "pagenums"))
		 (set-titlepage-pagenums (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "printhistory"))
		 (set-titlepage-printhistory (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "productname"))
		 (set-titlepage-productname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "productnumber"))
		 (set-titlepage-productnumber (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "pubdate"))
		 (set-titlepage-pubdate (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "publisher"))
		 (set-titlepage-publisher (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "publishername"))
		 (set-titlepage-publishername (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "pubsnumber"))
		 (set-titlepage-pubsnumber (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "releaseinfo"))
		 (set-titlepage-releaseinfo (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "revhistory"))
		 (set-titlepage-revhistory (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "seriesinfo"))
		 (set-titlepage-seriesinfo (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "seriesvolnums"))
		 (set-titlepage-seriesvolnums (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "subjectset"))
		 (set-titlepage-subjectset (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "subtitle"))
		 (set-titlepage-subtitle (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "surname"))
		 (set-titlepage-surname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "title"))
		 (set-titlepage-title (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "titleabbrev"))
		 (set-titlepage-titleabbrev (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "volumenum"))
		 (set-titlepage-volumenum (node-list-first nl) side))
		(else
		 (set-titlepage-default (node-list-first nl) side)))
	      (loop (node-list-rest nl) (node-list-first nl))))))))

(define (set-titlepage-before node side)
  (if (equal? side 'recto)
      (cond
       ((equal? (gi node) (normalize "corpauthor"))
	(make paragraph
	  space-after: (* (HSIZE 5) %head-after-factor% 8)
	  (literal "\no-break-space;")))
       ((equal? (gi node) (normalize "authorgroup"))
	(if (have-sibling? (normalize "corpauthor") node)
	    (empty-sosofo)
	    (make paragraph
	      space-after: (* (HSIZE 5) %head-after-factor% 8)
	      (literal "\no-break-space;"))))
       ((equal? (gi node) (normalize "author"))
	(if (or (have-sibling? (normalize "corpauthor") node) 
		(have-sibling? (normalize "authorgroup") node))
	    (empty-sosofo)
	    (make paragraph
	      space-after: (* (HSIZE 5) %head-after-factor% 8)
	      (literal "\no-break-space;"))))
       (else (empty-sosofo)))
      (empty-sosofo)))

(define (set-titlepage-default node side)
  (let ((foo (debug (string-append "No set-titlepage-* for " (gi node) "!"))))
    (empty-sosofo)))

(define (set-titlepage-element node side)
  (if (equal? side 'recto)
      (with-mode set-titlepage-recto-mode
	(process-node-list node))
      (with-mode set-titlepage-verso-mode
	(process-node-list node))))

(define (set-titlepage-abbrev node side)
  (set-titlepage-element node side))
(define (set-titlepage-abstract node side)
  (set-titlepage-element node side))
(define (set-titlepage-address node side)
  (set-titlepage-element node side))
(define (set-titlepage-affiliation node side)
  (set-titlepage-element node side))
(define (set-titlepage-artpagenums node side)
  (set-titlepage-element node side))
(define (set-titlepage-author node side)
  (set-titlepage-element node side))
(define (set-titlepage-authorblurb node side)
  (set-titlepage-element node side))
(define (set-titlepage-authorgroup node side)
  (set-titlepage-element node side))
(define (set-titlepage-authorinitials node side)
  (set-titlepage-element node side))
(define (set-titlepage-bibliomisc node side)
  (set-titlepage-element node side))
(define (set-titlepage-biblioset node side)
  (set-titlepage node side))
(define (set-titlepage-bookbiblio node side)
  (set-titlepage node side))
(define (set-titlepage-citetitle node side)
  (set-titlepage-element node side))
(define (set-titlepage-collab node side)
  (set-titlepage-element node side))
(define (set-titlepage-confgroup node side)
  (set-titlepage-element node side))
(define (set-titlepage-contractnum node side)
  (set-titlepage-element node side))
(define (set-titlepage-contractsponsor node side)
  (set-titlepage-element node side))
(define (set-titlepage-contrib  node side)
  (set-titlepage-element node side))
(define (set-titlepage-copyright node side)
  (set-titlepage-element node side))

(define (set-titlepage-corpauthor node side)
  (if (equal? side 'recto)
      (set-titlepage-element node side)
      (if (first-sibling? node)
	  (make paragraph
	    (with-mode set-titlepage-verso-mode
	      (process-node-list
	       (select-elements (children (parent node))
				(normalize "corpauthor")))))
	  (empty-sosofo))))

(define (set-titlepage-corpname node side)
  (set-titlepage-element node side))
(define (set-titlepage-date node side)
  (set-titlepage-element node side))
(define (set-titlepage-edition node side)
  (set-titlepage-element node side))
(define (set-titlepage-editor node side)
  (set-titlepage-element node side))
(define (set-titlepage-firstname node side)
  (set-titlepage-element node side))
(define (set-titlepage-graphic node side)
  (set-titlepage-element node side))
(define (set-titlepage-honorific node side)
  (set-titlepage-element node side))
(define (set-titlepage-indexterm node side)
  (set-titlepage-element node side))
(define (set-titlepage-invpartnumber node side)
  (set-titlepage-element node side))
(define (set-titlepage-isbn node side)
  (set-titlepage-element node side))
(define (set-titlepage-issn node side)
  (set-titlepage-element node side))
(define (set-titlepage-issuenum node side)
  (set-titlepage-element node side))
(define (set-titlepage-itermset node side)
  (set-titlepage-element node side))
(define (set-titlepage-keywordset node side)
  (set-titlepage-element node side))
(define (set-titlepage-legalnotice node side)
  (set-titlepage-element node side))
(define (set-titlepage-lineage node side)
  (set-titlepage-element node side))
(define (set-titlepage-mediaobject node side)
  (set-titlepage-element node side))
(define (set-titlepage-modespec node side)
  (set-titlepage-element node side))
(define (set-titlepage-orgname node side)
  (set-titlepage-element node side))
(define (set-titlepage-othercredit node side)
  (set-titlepage-element node side))
(define (set-titlepage-othername node side)
  (set-titlepage-element node side))
(define (set-titlepage-pagenums node side)
  (set-titlepage-element node side))
(define (set-titlepage-printhistory node side)
  (set-titlepage-element node side))
(define (set-titlepage-productname node side)
  (set-titlepage-element node side))
(define (set-titlepage-productnumber node side)
  (set-titlepage-element node side))
(define (set-titlepage-pubdate node side)
  (set-titlepage-element node side))
(define (set-titlepage-publisher node side)
  (set-titlepage-element node side))
(define (set-titlepage-publishername node side)
  (set-titlepage-element node side))
(define (set-titlepage-pubsnumber node side)
  (set-titlepage-element node side))
(define (set-titlepage-releaseinfo node side)
  (set-titlepage-element node side))
(define (set-titlepage-revhistory node side)
  (set-titlepage-element node side))
(define (set-titlepage-seriesinfo node side)
  (set-titlepage-element node side))
(define (set-titlepage-seriesvolnums node side)
  (set-titlepage-element node side))
(define (set-titlepage-subjectset node side)
  (set-titlepage-element node side))
(define (set-titlepage-subtitle node side)
  (set-titlepage-element node side))
(define (set-titlepage-surname node side)
  (set-titlepage-element node side))
(define (set-titlepage-title node side)
  (set-titlepage-element node side))
(define (set-titlepage-titleabbrev node side)
  (set-titlepage-element node side))
(define (set-titlepage-volumenum node side)
  (set-titlepage-element node side))

(define set-titlepage-recto-style
  (style
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: (HSIZE 1)))

(define set-titlepage-verso-style
  (style
      font-family-name: %body-font-family%))

(mode set-titlepage-recto-mode
  (element abbrev
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element abstract
    (make display-group
      use: set-titlepage-recto-style
      quadding: 'start
      ($semiformal-object$)))

  (element (abstract title) (empty-sosofo))

  (element (abstract para)
    (make paragraph
      use: set-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element address 
    (make display-group
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make display-group
      use: set-titlepage-recto-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element author
    (let ((author-name  (author-string))
	  (author-affil (select-elements (children (current-node)) 
					 (normalize "affiliation"))))
      (make sequence      
	(make paragraph
          use: set-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-before: (* (HSIZE 2) %head-before-factor%)
	  quadding: %division-title-quadding%
	  keep-with-next?: #t
	  (literal author-name))
	(process-node-list author-affil))))

  (element authorblurb
    (make display-group
      use: set-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element (authorblurb para)
    (make paragraph
      use: set-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element authorgroup
    (make display-group
      (process-children)))

  (element authorinitials
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element confgroup
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contractnum
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contrib
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element copyright
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))
  
  (element (copyright holder) ($charseq$))

  (element corpauthor
    (make sequence
      (make paragraph
	use: set-titlepage-recto-style
	font-size: (HSIZE 3)
	line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	space-before: (* (HSIZE 2) %head-before-factor%)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	(process-children))
      ;; This paragraph is a hack to get the spacing right.
      ;; Authors always have an affiliation paragraph below them, even if
      ;; it's empty, so corpauthors need one too.
      (make paragraph
	use: set-titlepage-recto-style
	font-size: (HSIZE 1)
	line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	space-after: (* (HSIZE 2) %head-after-factor% 4)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	(literal "\no-break-space;"))))

  (element corpname
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element date
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element edition
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    (let ((editor-name (author-string)))
      (make sequence      
	(if (first-sibling?) 
	    (make paragraph
	      use: set-titlepage-recto-style
	      font-size: (HSIZE 1)
	      line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	      space-before: (* (HSIZE 2) %head-before-factor% 6)
	      quadding: %division-title-quadding%
	      keep-with-next?: #t
	      (literal (gentext-edited-by)))
	    (empty-sosofo))
	(make paragraph
	  use: set-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-after: (* (HSIZE 2) %head-after-factor% 4)
	  quadding: %division-title-quadding%
	  keep-with-next?: #t
	  (literal editor-name)))))

  (element firstname
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'center)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element isbn
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element issn
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element issuenum
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element jobtitle
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: set-titlepage-recto-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para)
    (make paragraph
      use: set-titlepage-recto-style
      quadding: 'start
      line-spacing: (* 0.8 (inherited-line-spacing))
      font-size: (* 0.8 (inherited-font-size))
      (process-children)))

  (element lineage
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element orgname
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element othercredit
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element othername
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pagenums
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element printhistory
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element productname
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element productnumber
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pubdate
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element publisher
    (make display-group
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element publishername
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: set-titlepage-recto-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: set-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: set-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: set-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: set-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo)))))))
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element shortaffil
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle 
    (make paragraph
      use: set-titlepage-recto-style
      font-size: (HSIZE 4)
      line-spacing: (* (HSIZE 4) %line-spacing-factor%)
      space-before: (* (HSIZE 4) %head-before-factor%)
      quadding: %division-subtitle-quadding%
      keep-with-next?: #t
      (process-children-trim)))

  (element surname
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element title 
    (make paragraph
      use: set-titlepage-recto-style
      font-size: (HSIZE 5)
      line-spacing: (* (HSIZE 5) %line-spacing-factor%)
      space-before: (* (HSIZE 5) %head-before-factor%)
      quadding: %division-title-quadding%
      keep-with-next?: #t
      heading-level: (if %generate-heading-level% 1 0)
      (with-mode title-mode
	(process-children-trim))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: set-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))
)

(mode set-titlepage-verso-mode
  (element abbrev
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element abstract ($semiformal-object$))

  (element (abstract title) (empty-sosofo))

  (element address 
    (make display-group
      use: set-titlepage-verso-style
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element author
    ;; Print the author name.  Handle the case where there's no AUTHORGROUP
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (not in-group)
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-by))
	      (literal "\no-break-space;")
	      (literal (author-list-string))))
	  (make sequence 
	    (literal (author-list-string))))))  

  (element authorblurb
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element authorgroup
    (make paragraph
      space-after: (* %bf-size% %line-spacing-factor%)
      (make sequence
	(literal (gentext-by))
	(literal "\no-break-space;")
	(process-children-trim))))

  (element authorinitials
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element confgroup
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element contractnum
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element contrib
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element copyright
    (make paragraph
      use: set-titlepage-verso-style
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    ;; note: set-titlepage-corpauthor takes care of wrapping multiple
    ;; corpauthors
    (make sequence
      (if (first-sibling?)
	  (if (equal? (gi (parent (current-node))) (normalize "authorgroup"))
	      (empty-sosofo)
	      (literal (gentext-by) " "))
	  (literal ", "))
      (process-children)))

  (element corpname
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element date
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element edition
    (make paragraph
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    ;; Print the editor name.
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (or #t (not in-group)) ; nevermind, always put out the Edited by
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-edited-by))
	      (literal "\no-break-space;")
	      (literal (author-string))))
	  (make sequence 
	    (literal (author-string))))))

  (element firstname
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'start)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element isbn
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element issn
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element issuenum
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element jobtitle
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: set-titlepage-verso-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para) 
    (make paragraph
      use: set-titlepage-verso-style
      font-size: (* (inherited-font-size) 0.8)
      (process-children-trim)))

  (element lineage
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element orgname
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element othercredit
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element othername
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element pagenums
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element printhistory
    (make display-group
      use: set-titlepage-verso-style
      (process-children)))

  (element productname
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element productnumber
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element pubdate
    (make paragraph
      (literal (gentext-element-name-space (gi (current-node))))
      (process-children)))

  (element publisher
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element publishername
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: set-titlepage-verso-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: set-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: set-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: set-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: set-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo)))))))
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element shortaffil
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (literal (if (first-sibling?) ": " "; "))
      (process-children)))

  (element surname
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

  (element title
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (with-mode title-mode
	(process-children))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: set-titlepage-verso-style
      (process-children)))

)

;; == Title pages for BOOKs =============================================

(define (book-titlepage-recto-elements)
  (list (normalize "title") 
	(normalize "subtitle") 
	(normalize "graphic") 
	(normalize "mediaobject")
	(normalize "corpauthor") 
	(normalize "authorgroup") 
	(normalize "author") 
	(normalize "editor")))

(define (book-titlepage-verso-elements)
  (list (normalize "title") 
	(normalize "subtitle") 
	(normalize "corpauthor") 
	(normalize "authorgroup") 
	(normalize "author") 
	(normalize "editor")
	(normalize "edition") 
	(normalize "pubdate") 
	(normalize "copyright")
	(normalize "abstract") 
	(normalize "legalnotice") 
	(normalize "revhistory")))

(define (book-titlepage-content? elements side)
  (titlepage-content? elements (if (equal? side 'recto)
				   (book-titlepage-recto-elements)
				   (book-titlepage-verso-elements))))

(define (book-titlepage elements #!optional (side 'recto))
  (let ((nodelist (titlepage-nodelist 
		   (if (equal? side 'recto)
		       (book-titlepage-recto-elements)
		       (book-titlepage-verso-elements))
		   elements)))
;;    (make simple-page-sequence
;;      page-n-columns: %titlepage-n-columns%
;;      input-whitespace-treatment: 'collapse
;;      use: default-text-style
      (make sequence

      ;; This hack is required for the RTF backend. If an external-graphic
      ;; is the first thing on the page, RTF doesn't seem to do the right
      ;; thing (the graphic winds up on the baseline of the first line
      ;; of the page, left justified).  This "one point rule" fixes
      ;; that problem.
      (make paragraph
	line-spacing: 1pt
	(literal ""))
      
      (let loop ((nl nodelist) (lastnode (empty-node-list)))
	(if (node-list-empty? nl)
	    (empty-sosofo)
	    (make sequence
	      (if (or (node-list-empty? lastnode)
		      (not (equal? (gi (node-list-first nl))
				   (gi lastnode))))
		  (book-titlepage-before (node-list-first nl) side)
		  (empty-sosofo))
	      (cond
		((equal? (gi (node-list-first nl)) (normalize "abbrev"))
		 (book-titlepage-abbrev (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "abstract"))
		 (book-titlepage-abstract (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "address"))
		 (book-titlepage-address (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "affiliation"))
		 (book-titlepage-affiliation (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "artpagenums"))
		 (book-titlepage-artpagenums (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "author"))
		 (book-titlepage-author (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "authorblurb"))
		 (book-titlepage-authorblurb (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "authorgroup"))
		 (book-titlepage-authorgroup (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "authorinitials"))
		 (book-titlepage-authorinitials (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "bibliomisc"))
		 (book-titlepage-bibliomisc (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "biblioset"))
		 (book-titlepage-biblioset (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "bookbiblio"))
		 (book-titlepage-bookbiblio (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "citetitle"))
		 (book-titlepage-citetitle (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "collab"))
		 (book-titlepage-collab (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "confgroup"))
		 (book-titlepage-confgroup (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "contractnum"))
		 (book-titlepage-contractnum (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "contractsponsor"))
		 (book-titlepage-contractsponsor (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "contrib"))
		 (book-titlepage-contrib (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "copyright"))
		 (book-titlepage-copyright (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "corpauthor"))
		 (book-titlepage-corpauthor (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "corpname"))
		 (book-titlepage-corpname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "date"))
		 (book-titlepage-date (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "edition"))
		 (book-titlepage-edition (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "editor"))
		 (book-titlepage-editor (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "firstname"))
		 (book-titlepage-firstname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "graphic"))
		 (book-titlepage-graphic (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "honorific"))
		 (book-titlepage-honorific (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "indexterm"))
		 (book-titlepage-indexterm (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "invpartnumber"))
		 (book-titlepage-invpartnumber (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "isbn"))
		 (book-titlepage-isbn (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "issn"))
		 (book-titlepage-issn (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "issuenum"))
		 (book-titlepage-issuenum (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "itermset"))
		 (book-titlepage-itermset (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "keywordset"))
		 (book-titlepage-keywordset (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "legalnotice"))
		 (book-titlepage-legalnotice (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "lineage"))
		 (book-titlepage-lineage (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "mediaobject"))
		 (book-titlepage-mediaobject (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "modespec"))
		 (book-titlepage-modespec (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "orgname"))
		 (book-titlepage-orgname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "othercredit"))
		 (book-titlepage-othercredit (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "othername"))
		 (book-titlepage-othername (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "pagenums"))
		 (book-titlepage-pagenums (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "printhistory"))
		 (book-titlepage-printhistory (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "productname"))
		 (book-titlepage-productname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "productnumber"))
		 (book-titlepage-productnumber (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "pubdate"))
		 (book-titlepage-pubdate (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "publisher"))
		 (book-titlepage-publisher (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "publishername"))
		 (book-titlepage-publishername (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "pubsnumber"))
		 (book-titlepage-pubsnumber (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "releaseinfo"))
		 (book-titlepage-releaseinfo (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "revhistory"))
		 (book-titlepage-revhistory (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "seriesinfo"))
		 (book-titlepage-seriesinfo (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "seriesvolnums"))
		 (book-titlepage-seriesvolnums (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "subjectset"))
		 (book-titlepage-subjectset (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "subtitle"))
		 (book-titlepage-subtitle (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "surname"))
		 (book-titlepage-surname (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "title"))
		 (book-titlepage-title (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "titleabbrev"))
		 (book-titlepage-titleabbrev (node-list-first nl) side))
		((equal? (gi (node-list-first nl)) (normalize "volumenum"))
		 (book-titlepage-volumenum (node-list-first nl) side))
		(else
		 (book-titlepage-default (node-list-first nl) side)))
	      (loop (node-list-rest nl) (node-list-first nl))))))))

(define (book-titlepage-before node side)
  (if (equal? side 'recto)
      (cond
       ((equal? (gi node) (normalize "corpauthor"))
	(make paragraph
	  space-after: (* (HSIZE 5) %head-after-factor% 8)
	  (literal "\no-break-space;")))
       ((equal? (gi node) (normalize "authorgroup"))
	(if (have-sibling? (normalize "corpauthor") node)
	    (empty-sosofo)
	    (make paragraph
	      space-after: (* (HSIZE 5) %head-after-factor% 8)
	      (literal "\no-break-space;"))))
       ((equal? (gi node) (normalize "author"))
	(if (or (have-sibling? (normalize "corpauthor") node) 
		(have-sibling? (normalize "authorgroup") node))
	    (empty-sosofo)
	    (make paragraph
	      space-after: (* (HSIZE 5) %head-after-factor% 8)
	      (literal "\no-break-space;"))))
       (else (empty-sosofo)))
      (empty-sosofo)))

(define (book-titlepage-default node side)
  (let ((foo (debug (string-append "No book-titlepage-* for " (gi node) "!"))))
    (empty-sosofo)))

(define (book-titlepage-element node side)
  (if (equal? side 'recto)
      (with-mode book-titlepage-recto-mode
	(process-node-list node))
      (with-mode book-titlepage-verso-mode
	(process-node-list node))))

(define (book-titlepage-abbrev node side)
  (book-titlepage-element node side))
(define (book-titlepage-abstract node side)
  (book-titlepage-element node side))
(define (book-titlepage-address node side)
  (book-titlepage-element node side))
(define (book-titlepage-affiliation node side)
  (book-titlepage-element node side))
(define (book-titlepage-artpagenums node side)
  (book-titlepage-element node side))
(define (book-titlepage-author node side)
  (book-titlepage-element node side))
(define (book-titlepage-authorblurb node side)
  (book-titlepage-element node side))
(define (book-titlepage-authorgroup node side)
  (book-titlepage-element node side))
(define (book-titlepage-authorinitials node side)
  (book-titlepage-element node side))
(define (book-titlepage-bibliomisc node side)
  (book-titlepage-element node side))
(define (book-titlepage-biblioset node side)
  (book-titlepage node side))
(define (book-titlepage-bookbiblio node side)
  (book-titlepage node side))
(define (book-titlepage-citetitle node side)
  (book-titlepage-element node side))
(define (book-titlepage-collab node side)
  (book-titlepage-element node side))
(define (book-titlepage-confgroup node side)
  (book-titlepage-element node side))
(define (book-titlepage-contractnum node side)
  (book-titlepage-element node side))
(define (book-titlepage-contractsponsor node side)
  (book-titlepage-element node side))
(define (book-titlepage-contrib  node side)
  (book-titlepage-element node side))
(define (book-titlepage-copyright node side)
  (book-titlepage-element node side))

(define (book-titlepage-corpauthor node side)
  (if (equal? side 'recto)
      (book-titlepage-element node side)
      (if (first-sibling? node)
	  (make paragraph
	    (with-mode book-titlepage-verso-mode
	      (process-node-list
	       (select-elements (children (parent node)) 
				(normalize "corpauthor")))))
	  (empty-sosofo))))

(define (book-titlepage-corpname node side)
  (book-titlepage-element node side))
(define (book-titlepage-date node side)
  (book-titlepage-element node side))
(define (book-titlepage-edition node side)
  (book-titlepage-element node side))
(define (book-titlepage-editor node side)
  (book-titlepage-element node side))
(define (book-titlepage-firstname node side)
  (book-titlepage-element node side))
(define (book-titlepage-graphic node side)
  (book-titlepage-element node side))
(define (book-titlepage-honorific node side)
  (book-titlepage-element node side))
(define (book-titlepage-indexterm node side)
  (book-titlepage-element node side))
(define (book-titlepage-invpartnumber node side)
  (book-titlepage-element node side))
(define (book-titlepage-isbn node side)
  (book-titlepage-element node side))
(define (book-titlepage-issn node side)
  (book-titlepage-element node side))
(define (book-titlepage-issuenum node side)
  (book-titlepage-element node side))
(define (book-titlepage-itermset node side)
  (book-titlepage-element node side))
(define (book-titlepage-keywordset node side)
  (book-titlepage-element node side))
(define (book-titlepage-legalnotice node side)
  (book-titlepage-element node side))
(define (book-titlepage-lineage node side)
  (book-titlepage-element node side))
(define (book-titlepage-mediaobject node side)
  (book-titlepage-element node side))
(define (book-titlepage-modespec node side)
  (book-titlepage-element node side))
(define (book-titlepage-orgname node side)
  (book-titlepage-element node side))
(define (book-titlepage-othercredit node side)
  (book-titlepage-element node side))
(define (book-titlepage-othername node side)
  (book-titlepage-element node side))
(define (book-titlepage-pagenums node side)
  (book-titlepage-element node side))
(define (book-titlepage-printhistory node side)
  (book-titlepage-element node side))
(define (book-titlepage-productname node side)
  (book-titlepage-element node side))
(define (book-titlepage-productnumber node side)
  (book-titlepage-element node side))
(define (book-titlepage-pubdate node side)
  (book-titlepage-element node side))
(define (book-titlepage-publisher node side)
  (book-titlepage-element node side))
(define (book-titlepage-publishername node side)
  (book-titlepage-element node side))
(define (book-titlepage-pubsnumber node side)
  (book-titlepage-element node side))
(define (book-titlepage-releaseinfo node side)
  (book-titlepage-element node side))
(define (book-titlepage-revhistory node side)
  (book-titlepage-element node side))
(define (book-titlepage-seriesinfo node side)
  (book-titlepage-element node side))
(define (book-titlepage-seriesvolnums node side)
  (book-titlepage-element node side))
(define (book-titlepage-subjectset node side)
  (book-titlepage-element node side))
(define (book-titlepage-subtitle node side)
  (book-titlepage-element node side))
(define (book-titlepage-surname node side)
  (book-titlepage-element node side))
(define (book-titlepage-title node side)
  (book-titlepage-element node side))
(define (book-titlepage-titleabbrev node side)
  (book-titlepage-element node side))
(define (book-titlepage-volumenum node side)
  (book-titlepage-element node side))

(define book-titlepage-recto-style
  (style
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: (HSIZE 1)))

(define book-titlepage-verso-style
  (style
      font-family-name: %body-font-family%))

(mode book-titlepage-recto-mode
  (element abbrev
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element abstract
    (make display-group
      use: book-titlepage-recto-style
      quadding: 'start
      ($semiformal-object$)))

  (element (abstract title) (empty-sosofo))

  (element (abstract para)
    (make paragraph
      use: book-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element address 
    (make display-group
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make display-group
      use: book-titlepage-recto-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element author
    (let ((author-name  (author-string))
	  (author-affil (select-elements (children (current-node)) 
					 (normalize "affiliation"))))
      (make sequence      
	(make paragraph
	  use: book-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-before: (* (HSIZE 2) %head-before-factor%)
	  quadding: %division-title-quadding%
	  keep-with-next?: #t
	  (literal author-name))
	(process-node-list author-affil))))

  (element authorblurb
    (make display-group
      use: book-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element (authorblurb para)
    (make paragraph
      use: book-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element authorgroup
    (make display-group
      (process-children)))

  (element authorinitials
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element confgroup
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contractnum
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contrib
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element copyright
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    (make sequence
      (make paragraph
	use: book-titlepage-recto-style
	font-size: (HSIZE 3)
	line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	space-before: (* (HSIZE 2) %head-before-factor%)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	(process-children))
      ;; This paragraph is a hack to get the spacing right.
      ;; Authors always have an affiliation paragraph below them, even if
      ;; it's empty, so corpauthors need one too.
      (make paragraph
	use: book-titlepage-recto-style
	font-size: (HSIZE 1)
	line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	space-after: (* (HSIZE 2) %head-after-factor% 4)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	(literal "\no-break-space;"))))

  (element corpname
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element date
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element edition
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    (let ((editor-name (author-string)))
      (make sequence      
	(if (first-sibling?) 
	    (make paragraph
	      use: book-titlepage-recto-style
	      font-size: (HSIZE 1)
	      line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	      space-before: (* (HSIZE 2) %head-before-factor% 6)
	      quadding: %division-title-quadding%
	      keep-with-next?: #t
	      (literal (gentext-edited-by)))
	    (empty-sosofo))
	(make paragraph
	  use: book-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-after: (* (HSIZE 2) %head-after-factor% 4)
	  quadding: %division-title-quadding%
	  keep-with-next?: #t
	  (literal editor-name)))))

  (element firstname
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'center)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element isbn
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element issn
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element issuenum
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element jobtitle
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: book-titlepage-recto-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para)
    (make paragraph
      use: book-titlepage-recto-style
      quadding: 'start
      line-spacing: (* 0.8 (inherited-line-spacing))
      font-size: (* 0.8 (inherited-font-size))
      (process-children)))

  (element lineage
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element orgname
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element othercredit
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element othername
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pagenums
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element printhistory
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element productname
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element productnumber
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pubdate
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element publisher
    (make display-group
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element publishername
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: book-titlepage-recto-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: book-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: book-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: book-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: book-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo)))))))
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element shortaffil
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle 
    (make paragraph
      use: book-titlepage-recto-style
      font-size: (HSIZE 4)
      line-spacing: (* (HSIZE 4) %line-spacing-factor%)
      space-before: (* (HSIZE 4) %head-before-factor%)
      quadding: %division-subtitle-quadding%
      keep-with-next?: #t
      (process-children-trim)))

  (element surname
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element title 
    (make paragraph
      use: book-titlepage-recto-style
      font-size: (HSIZE 5)
      line-spacing: (* (HSIZE 5) %line-spacing-factor%)
      space-before: (* (HSIZE 5) %head-before-factor%)
      quadding: %division-title-quadding%
      keep-with-next?: #t
      heading-level: (if %generate-heading-level% 1 0)
      (with-mode title-mode
	(process-children-trim))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: book-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))
)

(mode book-titlepage-verso-mode
  (element abbrev
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element abstract ($semiformal-object$))

  (element (abstract title) (empty-sosofo))

  (element address 
    (make display-group
      use: book-titlepage-verso-style
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element author
    ;; Print the author name.  Handle the case where there's no AUTHORGROUP
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (not in-group)
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-by))
	      (literal "\no-break-space;")
	      (literal (author-list-string))))
	  (make sequence 
	    (literal (author-list-string))))))  

  (element authorblurb
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element authorgroup
    (let* ((editors (select-elements (children (current-node)) (normalize "editor"))))
      (make paragraph
	space-after: (* %bf-size% %line-spacing-factor%)
	(make sequence
	  (if (node-list-empty? editors)
	      (literal (gentext-by))
	      (literal (gentext-edited-by)))
	  (literal "\no-break-space;")
	  (process-children-trim)))))

  (element authorinitials
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element confgroup
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element contractnum
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element contrib
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element copyright
    (make paragraph
      use: book-titlepage-verso-style
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    ;; note: book-titlepage-corpauthor takes care of wrapping multiple
    ;; corpauthors
    (make sequence
      (if (first-sibling?)
	  (if (equal? (gi (parent (current-node))) (normalize "authorgroup"))
	      (empty-sosofo)
	      (literal (gentext-by) " "))
	  (literal ", "))
      (process-children)))

  (element corpname
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element date
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element edition
    (make paragraph
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    ;; Print the editor name.
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (or #f (not in-group)) ; nevermind, always put out the Edited by
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-edited-by))
	      (literal "\no-break-space;")
	      (literal (author-string))))
	  (make sequence
	    (literal (author-list-string))))))

  (element firstname
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'start)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element isbn
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element issn
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element issuenum
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element jobtitle
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: book-titlepage-verso-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para) 
    (make paragraph
      use: book-titlepage-verso-style
      font-size: (* (inherited-font-size) 0.8)
      (process-children-trim)))

  (element lineage
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element orgname
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element othercredit
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element othername
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element pagenums
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element printhistory
    (make display-group
      use: book-titlepage-verso-style
      (process-children)))

  (element productname
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element productnumber
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element pubdate
    (make paragraph
      (literal (gentext-element-name-space (gi (current-node))))
      (process-children)))

  (element publisher
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element publishername
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: book-titlepage-verso-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: book-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: book-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: book-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: book-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo))))))) 
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element shortaffil
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (literal (if (first-sibling?) ": " "; "))
      (process-children)))

  (element surname
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))

  (element title
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (with-mode title-mode
	(process-children))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: book-titlepage-verso-style
      (process-children)))
)

;; == Title pages for PARTs =============================================

(define (part-titlepage-recto-elements)
  (list (normalize "title")
	(normalize "subtitle")))

(define (part-titlepage-verso-elements)
  '())

(define (part-titlepage-content? elements side)
  (titlepage-content? elements (if (equal? side 'recto)
				   (part-titlepage-recto-elements)
				   (part-titlepage-verso-elements))))

(define (part-titlepage elements #!optional (side 'recto))
  (let ((nodelist (titlepage-nodelist 
		   (if (equal? side 'recto)
		       (part-titlepage-recto-elements)
		       (part-titlepage-verso-elements))
		   elements))
        ;; partintro is a special case...
	(partintro (node-list-first
		    (node-list-filter-by-gi elements (list (normalize "partintro"))))))
    (if (part-titlepage-content? elements side)
;;	(make simple-page-sequence
;;	  page-n-columns: %titlepage-n-columns%
;;	  input-whitespace-treatment: 'collapse
;;	  use: default-text-style
	(make sequence
  
	  ;; This hack is required for the RTF backend. If an external-graphic
	  ;; is the first thing on the page, RTF doesn't seem to do the right
	  ;; thing (the graphic winds up on the baseline of the first line
	  ;; of the page, left justified).  This "one point rule" fixes
	  ;; that problem.
	  (make paragraph
	    line-spacing: 1pt
	    (literal ""))

	  (let loop ((nl nodelist) (lastnode (empty-node-list)))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(make sequence
		  (if (or (node-list-empty? lastnode)
			  (not (equal? (gi (node-list-first nl))
				       (gi lastnode))))
		      (part-titlepage-before (node-list-first nl) side)
		      (empty-sosofo))
		  (cond
		   ((equal? (gi (node-list-first nl)) (normalize "abbrev"))
		    (part-titlepage-abbrev (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "abstract"))
		    (part-titlepage-abstract (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "address"))
		    (part-titlepage-address (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "affiliation"))
		    (part-titlepage-affiliation (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "artpagenums"))
		    (part-titlepage-artpagenums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "author"))
		    (part-titlepage-author (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorblurb"))
		    (part-titlepage-authorblurb (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorgroup"))
		    (part-titlepage-authorgroup (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorinitials"))
		    (part-titlepage-authorinitials (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "bibliomisc"))
		    (part-titlepage-bibliomisc (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "biblioset"))
		    (part-titlepage-biblioset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "bookbiblio"))
		    (part-titlepage-bookbiblio (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "citetitle"))
		    (part-titlepage-citetitle (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "collab"))
		    (part-titlepage-collab (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "confgroup"))
		    (part-titlepage-confgroup (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contractnum"))
		    (part-titlepage-contractnum (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contractsponsor"))
		    (part-titlepage-contractsponsor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contrib"))
		    (part-titlepage-contrib (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "copyright"))
		    (part-titlepage-copyright (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "corpauthor"))
		    (part-titlepage-corpauthor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "corpname"))
		    (part-titlepage-corpname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "date"))
		    (part-titlepage-date (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "edition"))
		    (part-titlepage-edition (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "editor"))
		    (part-titlepage-editor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "firstname"))
		    (part-titlepage-firstname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "graphic"))
		    (part-titlepage-graphic (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "honorific"))
		    (part-titlepage-honorific (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "indexterm"))
		    (part-titlepage-indexterm (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "invpartnumber"))
		    (part-titlepage-invpartnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "isbn"))
		    (part-titlepage-isbn (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "issn"))
		    (part-titlepage-issn (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "issuenum"))
		    (part-titlepage-issuenum (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "itermset"))
		    (part-titlepage-itermset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "keywordset"))
		    (part-titlepage-keywordset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "legalnotice"))
		    (part-titlepage-legalnotice (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "lineage"))
		    (part-titlepage-lineage (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "mediaobject"))
		    (part-titlepage-mediaobject (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "modespec"))
		    (part-titlepage-modespec (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "orgname"))
		    (part-titlepage-orgname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "othercredit"))
		    (part-titlepage-othercredit (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "othername"))
		    (part-titlepage-othername (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pagenums"))
		    (part-titlepage-pagenums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "printhistory"))
		    (part-titlepage-printhistory (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "productname"))
		    (part-titlepage-productname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "productnumber"))
		    (part-titlepage-productnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pubdate"))
		    (part-titlepage-pubdate (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "publisher"))
		    (part-titlepage-publisher (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "publishername"))
		    (part-titlepage-publishername (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pubsnumber"))
		    (part-titlepage-pubsnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "releaseinfo"))
		    (part-titlepage-releaseinfo (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "revhistory"))
		    (part-titlepage-revhistory (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "seriesinfo"))
		    (part-titlepage-seriesinfo (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "seriesvolnums"))
		    (part-titlepage-seriesvolnums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "subjectset"))
		    (part-titlepage-subjectset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "subtitle"))
		    (part-titlepage-subtitle (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "surname"))
		    (part-titlepage-surname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "title"))
		    (part-titlepage-title (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "titleabbrev"))
		    (part-titlepage-titleabbrev (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "volumenum"))
		    (part-titlepage-volumenum (node-list-first nl) side))
		   (else
		    (part-titlepage-default (node-list-first nl) side)))
		  (loop (node-list-rest nl) (node-list-first nl)))))

	  (if (and %generate-part-toc%
		   %generate-part-toc-on-titlepage%
		   (equal? side 'recto))
	      (make display-group
		(build-toc (current-node)
			   (toc-depth (current-node))))
	      (empty-sosofo))

	  ;; PartIntro is a special case
	  (if (and (equal? side 'recto)
		   (not (node-list-empty? partintro))
		   %generate-partintro-on-titlepage%)
	      ($process-partintro$ partintro #f)
	      (empty-sosofo)))
	(empty-sosofo))))

(define (part-titlepage-before node side)
  (if (equal? side 'recto)
      (cond
       ((equal? (gi node) (normalize "corpauthor"))
	(make paragraph
	  space-after: (* (HSIZE 5) %head-after-factor% 8)
	  (literal "\no-break-space;")))
       ((equal? (gi node) (normalize "authorgroup"))
	(if (have-sibling? (normalize "corpauthor") node)
	    (empty-sosofo)
	    (make paragraph
	      space-after: (* (HSIZE 5) %head-after-factor% 8)
	      (literal "\no-break-space;"))))
       ((equal? (gi node) (normalize "author"))
	(if (or (have-sibling? (normalize "corpauthor") node) 
		(have-sibling? (normalize "authorgroup") node))
	    (empty-sosofo)
	    (make paragraph
	      space-after: (* (HSIZE 5) %head-after-factor% 8)
	      (literal "\no-break-space;"))))
       (else (empty-sosofo)))
      (empty-sosofo)))

(define (part-titlepage-default node side)
  (let ((foo (debug (string-append "No part-titlepage-* for " (gi node) "!"))))
    (empty-sosofo)))

(define (part-titlepage-element node side)
  (if (equal? side 'recto)
      (with-mode part-titlepage-recto-mode
	(process-node-list node))
      (with-mode part-titlepage-verso-mode
	(process-node-list node))))

(define (part-titlepage-abbrev node side)
  (part-titlepage-element node side))
(define (part-titlepage-abstract node side)
  (part-titlepage-element node side))
(define (part-titlepage-address node side)
  (part-titlepage-element node side))
(define (part-titlepage-affiliation node side)
  (part-titlepage-element node side))
(define (part-titlepage-artpagenums node side)
  (part-titlepage-element node side))
(define (part-titlepage-author node side)
  (part-titlepage-element node side))
(define (part-titlepage-authorblurb node side)
  (part-titlepage-element node side))
(define (part-titlepage-authorgroup node side)
  (part-titlepage-element node side))
(define (part-titlepage-authorinitials node side)
  (part-titlepage-element node side))
(define (part-titlepage-bibliomisc node side)
  (part-titlepage-element node side))
(define (part-titlepage-biblioset node side)
  (part-titlepage node side))
(define (part-titlepage-bookbiblio node side)
  (part-titlepage node side))
(define (part-titlepage-citetitle node side)
  (part-titlepage-element node side))
(define (part-titlepage-collab node side)
  (part-titlepage-element node side))
(define (part-titlepage-confgroup node side)
  (part-titlepage-element node side))
(define (part-titlepage-contractnum node side)
  (part-titlepage-element node side))
(define (part-titlepage-contractsponsor node side)
  (part-titlepage-element node side))
(define (part-titlepage-contrib  node side)
  (part-titlepage-element node side))
(define (part-titlepage-copyright node side)
  (part-titlepage-element node side))

(define (part-titlepage-corpauthor node side)
  (if (equal? side 'recto)
      (book-titlepage-element node side)
      (if (first-sibling? node)
	  (make paragraph
	    (with-mode book-titlepage-verso-mode
	      (process-node-list
	       (select-elements (children (parent node)) 
				(normalize "corpauthor")))))
	  (empty-sosofo))))

(define (part-titlepage-corpname node side)
  (part-titlepage-element node side))
(define (part-titlepage-date node side)
  (part-titlepage-element node side))
(define (part-titlepage-edition node side)
  (part-titlepage-element node side))
(define (part-titlepage-editor node side)
  (part-titlepage-element node side))
(define (part-titlepage-firstname node side)
  (part-titlepage-element node side))
(define (part-titlepage-graphic node side)
  (part-titlepage-element node side))
(define (part-titlepage-honorific node side)
  (part-titlepage-element node side))
(define (part-titlepage-indexterm node side)
  (part-titlepage-element node side))
(define (part-titlepage-invpartnumber node side)
  (part-titlepage-element node side))
(define (part-titlepage-isbn node side)
  (part-titlepage-element node side))
(define (part-titlepage-issn node side)
  (part-titlepage-element node side))
(define (part-titlepage-issuenum node side)
  (part-titlepage-element node side))
(define (part-titlepage-itermset node side)
  (part-titlepage-element node side))
(define (part-titlepage-keywordset node side)
  (part-titlepage-element node side))
(define (part-titlepage-legalnotice node side)
  (part-titlepage-element node side))
(define (part-titlepage-lineage node side)
  (part-titlepage-element node side))
(define (part-titlepage-mediaobject node side)
  (part-titlepage-element node side))
(define (part-titlepage-modespec node side)
  (part-titlepage-element node side))
(define (part-titlepage-orgname node side)
  (part-titlepage-element node side))
(define (part-titlepage-othercredit node side)
  (part-titlepage-element node side))
(define (part-titlepage-othername node side)
  (part-titlepage-element node side))
(define (part-titlepage-pagenums node side)
  (part-titlepage-element node side))
(define (part-titlepage-printhistory node side)
  (part-titlepage-element node side))
(define (part-titlepage-productname node side)
  (part-titlepage-element node side))
(define (part-titlepage-productnumber node side)
  (part-titlepage-element node side))
(define (part-titlepage-pubdate node side)
  (part-titlepage-element node side))
(define (part-titlepage-publisher node side)
  (part-titlepage-element node side))
(define (part-titlepage-publishername node side)
  (part-titlepage-element node side))
(define (part-titlepage-pubsnumber node side)
  (part-titlepage-element node side))
(define (part-titlepage-releaseinfo node side)
  (part-titlepage-element node side))
(define (part-titlepage-revhistory node side)
  (part-titlepage-element node side))
(define (part-titlepage-seriesinfo node side)
  (part-titlepage-element node side))
(define (part-titlepage-seriesvolnums node side)
  (part-titlepage-element node side))
(define (part-titlepage-subjectset node side)
  (part-titlepage-element node side))
(define (part-titlepage-subtitle node side)
  (part-titlepage-element node side))
(define (part-titlepage-surname node side)
  (part-titlepage-element node side))
(define (part-titlepage-title node side)
  (part-titlepage-element node side))
(define (part-titlepage-titleabbrev node side)
  (part-titlepage-element node side))
(define (part-titlepage-volumenum node side)
  (part-titlepage-element node side))

(define part-titlepage-recto-style
  (style
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: (HSIZE 1)))

(define part-titlepage-verso-style
  (style
      font-family-name: %body-font-family%))

(mode part-titlepage-recto-mode
  (element abbrev
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element abstract
    (make display-group
      use: part-titlepage-recto-style
      quadding: 'start
      ($semiformal-object$)))

  (element (abstract title) (empty-sosofo))

  (element (abstract para)
    (make paragraph
      use: part-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element address 
    (make display-group
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make display-group
      use: part-titlepage-recto-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element author
    (let ((author-name  (author-string))
	  (author-affil (select-elements (children (current-node)) 
					 (normalize "affiliation"))))
      (make sequence      
	(make paragraph
          use: part-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-before: (* (HSIZE 2) %head-before-factor%)
	  quadding: %division-title-quadding%
	  keep-with-next?: #t
	  (literal author-name))
	(process-node-list author-affil))))

  (element authorblurb
    (make display-group
      use: part-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element (authorblurb para)
    (make paragraph
      use: part-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element authorgroup
    (make display-group
      (process-children)))

  (element authorinitials
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element confgroup
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contractnum
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contrib
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element copyright
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    (make sequence
      (make paragraph
        use: part-titlepage-recto-style
	font-size: (HSIZE 3)
	line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	space-before: (* (HSIZE 2) %head-before-factor%)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	(process-children))
      ;; This paragraph is a hack to get the spacing right.
      ;; Authors always have an affiliation paragraph below them, even if
      ;; it's empty, so corpauthors need one too.
      (make paragraph
        use: part-titlepage-recto-style
	font-size: (HSIZE 1)
	line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	space-after: (* (HSIZE 2) %head-after-factor% 4)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	(literal "\no-break-space;"))))

  (element corpname
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element date
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element edition
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    (let ((editor-name (author-string)))
      (make sequence      
	(if (first-sibling?) 
	    (make paragraph
              use: part-titlepage-recto-style
	      font-size: (HSIZE 1)
	      line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	      space-before: (* (HSIZE 2) %head-before-factor% 6)
	      quadding: %division-title-quadding%
	      keep-with-next?: #t
	      (literal (gentext-edited-by)))
	    (empty-sosofo))
	(make paragraph
          use: part-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-after: (* (HSIZE 2) %head-after-factor% 4)
	  quadding: %division-title-quadding%
	  keep-with-next?: #t
	  (literal editor-name)))))

  (element firstname
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'center)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element isbn
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element issn
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element issuenum
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element jobtitle
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: part-titlepage-recto-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para)
    (make paragraph
      use: part-titlepage-recto-style
      quadding: 'start
      line-spacing: (* 0.8 (inherited-line-spacing))
      font-size: (* 0.8 (inherited-font-size))
      (process-children)))

  (element lineage
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element orgname
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element othercredit
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element othername
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pagenums
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element printhistory
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element productname
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element productnumber
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pubdate
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element publisher
    (make display-group
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element publishername
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: part-titlepage-recto-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: part-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: part-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: part-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: part-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo))))))) 
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element shortaffil
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle 
    (make paragraph
      use: part-titlepage-recto-style
      font-size: (HSIZE 4)
      line-spacing: (* (HSIZE 4) %line-spacing-factor%)
      space-before: (* (HSIZE 4) %head-before-factor%)
      quadding: %division-subtitle-quadding%
      keep-with-next?: #t
      (process-children-trim)))

  (element surname
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element title
    (let ((division (ancestor-member (current-node) (division-element-list))))
      (make paragraph
        use: part-titlepage-recto-style
	font-size: (HSIZE 5)
	line-spacing: (* (HSIZE 5) %line-spacing-factor%)
	space-before: (* (HSIZE 5) %head-before-factor%)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	heading-level: (if %generate-heading-level% 1 0)
	(if (string=? (element-label division) "")
	    (empty-sosofo)
	    (literal (element-label division) 
		     (gentext-label-title-sep (gi division))))
	(with-mode title-mode
	  (process-children)))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: part-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))
)

(mode part-titlepage-verso-mode
  (element abbrev
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element abstract ($semiformal-object$))

  (element (abstract title) (empty-sosofo))

  (element address 
    (make display-group
      use: part-titlepage-verso-style
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element author
    ;; Print the author name.  Handle the case where there's no AUTHORGROUP
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (not in-group)
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-by))
	      (literal "\no-break-space;")
	      (literal (author-list-string))))
	  (make sequence 
	    (literal (author-list-string))))))  

  (element authorblurb
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element authorgroup
    (make paragraph
      space-after: (* %bf-size% %line-spacing-factor%)
      (make sequence
	(literal (gentext-by))
	(literal "\no-break-space;")
	(process-children-trim))))

  (element authorinitials
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element confgroup
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element contractnum
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element contrib
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element copyright
    (make paragraph
      use: part-titlepage-verso-style
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    ;; note: part-titlepage-corpauthor takes care of wrapping multiple
    ;; corpauthors
    (make sequence
      (if (first-sibling?)
	  (if (equal? (gi (parent (current-node))) (normalize "authorgroup"))
	      (empty-sosofo)
	      (literal (gentext-by) " "))
	  (literal ", "))
      (process-children)))

  (element corpname
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element date
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element edition
    (make paragraph
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    ;; Print the editor name.
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (or #t (not in-group)) ; nevermind, always put out the Edited by
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-edited-by))
	      (literal "\no-break-space;")
	      (literal (author-string))))
	  (make sequence 
	    (literal (author-string))))))

  (element firstname
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'start)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element isbn
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element issn
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element issuenum
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element jobtitle
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: part-titlepage-verso-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para) 
    (make paragraph
      use: part-titlepage-verso-style
      font-size: (* (inherited-font-size) 0.8)
      (process-children-trim)))

  (element lineage
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element orgname
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element othercredit
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element othername
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element pagenums
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element printhistory
    (make display-group
      use: part-titlepage-verso-style
      (process-children)))

  (element productname
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element productnumber
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element pubdate
    (make paragraph
      (literal (gentext-element-name-space (gi (current-node))))
      (process-children)))

  (element publisher
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element publishername
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: part-titlepage-verso-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: part-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: part-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: part-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: part-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo))))))) 
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element shortaffil
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (literal (if (first-sibling?) ": " "; "))
      (process-children)))

  (element surname
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))

  (element title
    (let ((division (ancestor-member (current-node) (division-element-list))))
      (make sequence
        font-family-name: %title-font-family%
	font-weight: 'bold
	(if (string=? (element-label division) "")
	    (empty-sosofo)
	    (literal (element-label division) 
		     (gentext-label-title-sep (gi division))))
	(with-mode title-mode
	  (process-children)))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: part-titlepage-verso-style
      (process-children)))
)

;; == Title pages for ARTICLEs ==========================================
;;
;; Note: Article title pages are a little different in that they do not
;; create their own simple-page-sequence.
;;

(define (article-titlepage-recto-elements)
  (list (normalize "title") 
	(normalize "subtitle") 
	(normalize "corpauthor") 
	(normalize "authorgroup") 
	(normalize "author") 
	(normalize "abstract")))

(define (article-titlepage-verso-elements)
  '())

(define (article-titlepage-content? elements side)
  (titlepage-content? elements (if (equal? side 'recto)
				   (article-titlepage-recto-elements)
				   (article-titlepage-verso-elements))))

(define (article-titlepage elements #!optional (side 'recto))
  (let* ((nodelist (titlepage-nodelist 
		    (if (equal? side 'recto)
			(article-titlepage-recto-elements)
			(article-titlepage-verso-elements))
		    elements)))
    (if (article-titlepage-content? elements side)
	(make sequence
	  (let loop ((nl nodelist) (lastnode (empty-node-list)))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(make sequence
		  (if (or (node-list-empty? lastnode)
			  (not (equal? (gi (node-list-first nl))
				       (gi lastnode))))
		      (article-titlepage-before (node-list-first nl) side)
		      (empty-sosofo))
		  (cond
		   ((equal? (gi (node-list-first nl)) (normalize "abbrev"))
		    (article-titlepage-abbrev (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "abstract"))
		    (article-titlepage-abstract (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "address"))
		    (article-titlepage-address (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "affiliation"))
		    (article-titlepage-affiliation (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "artpagenums"))
		    (article-titlepage-artpagenums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "author"))
		    (article-titlepage-author (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorblurb"))
		    (article-titlepage-authorblurb (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorgroup"))
		    (article-titlepage-authorgroup (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorinitials"))
		    (article-titlepage-authorinitials (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "bibliomisc"))
		    (article-titlepage-bibliomisc (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "biblioset"))
		    (article-titlepage-biblioset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "bookbiblio"))
		    (article-titlepage-bookbiblio (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "citetitle"))
		    (article-titlepage-citetitle (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "collab"))
		    (article-titlepage-collab (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "confgroup"))
		    (article-titlepage-confgroup (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contractnum"))
		    (article-titlepage-contractnum (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contractsponsor"))
		    (article-titlepage-contractsponsor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contrib"))
		    (article-titlepage-contrib (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "copyright"))
		    (article-titlepage-copyright (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "corpauthor"))
		    (article-titlepage-corpauthor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "corpname"))
		    (article-titlepage-corpname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "date"))
		    (article-titlepage-date (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "edition"))
		    (article-titlepage-edition (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "editor"))
		    (article-titlepage-editor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "firstname"))
		    (article-titlepage-firstname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "graphic"))
		    (article-titlepage-graphic (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "honorific"))
		    (article-titlepage-honorific (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "indexterm"))
		    (article-titlepage-indexterm (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "invpartnumber"))
		    (article-titlepage-invpartnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "isbn"))
		    (article-titlepage-isbn (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "issn"))
		    (article-titlepage-issn (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "issuenum"))
		    (article-titlepage-issuenum (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "itermset"))
		    (article-titlepage-itermset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "keywordset"))
		    (article-titlepage-keywordset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "legalnotice"))
		    (article-titlepage-legalnotice (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "lineage"))
		    (article-titlepage-lineage (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "mediaobject"))
		    (article-titlepage-mediaobject (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "modespec"))
		    (article-titlepage-modespec (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "orgname"))
		    (article-titlepage-orgname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "othercredit"))
		    (article-titlepage-othercredit (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "othername"))
		    (article-titlepage-othername (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pagenums"))
		    (article-titlepage-pagenums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "printhistory"))
		    (article-titlepage-printhistory (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "productname"))
		    (article-titlepage-productname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "productnumber"))
		    (article-titlepage-productnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pubdate"))
		    (article-titlepage-pubdate (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "publisher"))
		    (article-titlepage-publisher (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "publishername"))
		    (article-titlepage-publishername (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pubsnumber"))
		    (article-titlepage-pubsnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "releaseinfo"))
		    (article-titlepage-releaseinfo (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "revhistory"))
		    (article-titlepage-revhistory (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "seriesinfo"))
		    (article-titlepage-seriesinfo (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "seriesvolnums"))
		    (article-titlepage-seriesvolnums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "subjectset"))
		    (article-titlepage-subjectset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "subtitle"))
		    (article-titlepage-subtitle (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "surname"))
		    (article-titlepage-surname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "title"))
		    (article-titlepage-title (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "titleabbrev"))
		    (article-titlepage-titleabbrev (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "volumenum"))
		    (article-titlepage-volumenum (node-list-first nl) side))
		   (else
		    (article-titlepage-default (node-list-first nl) side)))
		  (loop (node-list-rest nl) (node-list-first nl)))))

	  (if (and %generate-article-toc%
		   %generate-article-toc-on-titlepage%
		   (equal? side 'recto))
	      (make display-group
		(build-toc (current-node)
			   (toc-depth (current-node))))
	      (empty-sosofo)))
	(empty-sosofo))))

(define (article-titlepage-before node side)
  (empty-sosofo))

(define (article-titlepage-default node side)
  (let ((foo (debug (string-append "No article-titlepage-* for " (gi node) "!"))))
    (empty-sosofo)))

(define (article-titlepage-element node side)
  (if (equal? side 'recto)
      (with-mode article-titlepage-recto-mode
	(process-node-list node))
      (with-mode article-titlepage-verso-mode
	(process-node-list node))))

(define (article-titlepage-abbrev node side)
  (article-titlepage-element node side))
(define (article-titlepage-abstract node side)
  (article-titlepage-element node side))
(define (article-titlepage-address node side)
  (article-titlepage-element node side))
(define (article-titlepage-affiliation node side)
  (article-titlepage-element node side))
(define (article-titlepage-artpagenums node side)
  (article-titlepage-element node side))
(define (article-titlepage-author node side)
  (article-titlepage-element node side))
(define (article-titlepage-authorblurb node side)
  (article-titlepage-element node side))
(define (article-titlepage-authorgroup node side)
  (article-titlepage-element node side))
(define (article-titlepage-authorinitials node side)
  (article-titlepage-element node side))
(define (article-titlepage-bibliomisc node side)
  (article-titlepage-element node side))
(define (article-titlepage-biblioset node side)
  (article-titlepage node side))
(define (article-titlepage-bookbiblio node side)
  (article-titlepage node side))
(define (article-titlepage-citetitle node side)
  (article-titlepage-element node side))
(define (article-titlepage-collab node side)
  (article-titlepage-element node side))
(define (article-titlepage-confgroup node side)
  (article-titlepage-element node side))
(define (article-titlepage-contractnum node side)
  (article-titlepage-element node side))
(define (article-titlepage-contractsponsor node side)
  (article-titlepage-element node side))
(define (article-titlepage-contrib  node side)
  (article-titlepage-element node side))
(define (article-titlepage-copyright node side)
  (article-titlepage-element node side))

(define (article-titlepage-corpauthor node side)
  (if (equal? side 'recto)
      (book-titlepage-element node side)
      (if (first-sibling? node)
	  (make paragraph
	    (with-mode book-titlepage-verso-mode
	      (process-node-list
	       (select-elements (children (parent node)) 
				(normalize "corpauthor")))))
	  (empty-sosofo))))

(define (article-titlepage-corpname node side)
  (article-titlepage-element node side))
(define (article-titlepage-date node side)
  (article-titlepage-element node side))
(define (article-titlepage-edition node side)
  (article-titlepage-element node side))
(define (article-titlepage-editor node side)
  (article-titlepage-element node side))
(define (article-titlepage-firstname node side)
  (article-titlepage-element node side))
(define (article-titlepage-graphic node side)
  (article-titlepage-element node side))
(define (article-titlepage-honorific node side)
  (article-titlepage-element node side))
(define (article-titlepage-indexterm node side)
  (article-titlepage-element node side))
(define (article-titlepage-invpartnumber node side)
  (article-titlepage-element node side))
(define (article-titlepage-isbn node side)
  (article-titlepage-element node side))
(define (article-titlepage-issn node side)
  (article-titlepage-element node side))
(define (article-titlepage-issuenum node side)
  (article-titlepage-element node side))
(define (article-titlepage-itermset node side)
  (article-titlepage-element node side))
(define (article-titlepage-keywordset node side)
  (article-titlepage-element node side))
(define (article-titlepage-legalnotice node side)
  (article-titlepage-element node side))
(define (article-titlepage-lineage node side)
  (article-titlepage-element node side))
(define (article-titlepage-mediaobject node side)
  (article-titlepage-element node side))
(define (article-titlepage-modespec node side)
  (article-titlepage-element node side))
(define (article-titlepage-orgname node side)
  (article-titlepage-element node side))
(define (article-titlepage-othercredit node side)
  (article-titlepage-element node side))
(define (article-titlepage-othername node side)
  (article-titlepage-element node side))
(define (article-titlepage-pagenums node side)
  (article-titlepage-element node side))
(define (article-titlepage-printhistory node side)
  (article-titlepage-element node side))
(define (article-titlepage-productname node side)
  (article-titlepage-element node side))
(define (article-titlepage-productnumber node side)
  (article-titlepage-element node side))
(define (article-titlepage-pubdate node side)
  (article-titlepage-element node side))
(define (article-titlepage-publisher node side)
  (article-titlepage-element node side))
(define (article-titlepage-publishername node side)
  (article-titlepage-element node side))
(define (article-titlepage-pubsnumber node side)
  (article-titlepage-element node side))
(define (article-titlepage-releaseinfo node side)
  (article-titlepage-element node side))
(define (article-titlepage-revhistory node side)
  (article-titlepage-element node side))
(define (article-titlepage-seriesinfo node side)
  (article-titlepage-element node side))
(define (article-titlepage-seriesvolnums node side)
  (article-titlepage-element node side))
(define (article-titlepage-subjectset node side)
  (article-titlepage-element node side))
(define (article-titlepage-subtitle node side)
  (article-titlepage-element node side))
(define (article-titlepage-surname node side)
  (article-titlepage-element node side))
(define (article-titlepage-title node side)
  (article-titlepage-element node side))
(define (article-titlepage-titleabbrev node side)
  (article-titlepage-element node side))
(define (article-titlepage-volumenum node side)
  (article-titlepage-element node side))

(define article-titlepage-recto-style
  (style
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: (HSIZE 1)))

(define article-titlepage-verso-style
  (style
      font-family-name: %body-font-family%))

(mode article-titlepage-recto-mode
  (element abbrev
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element abstract
    (make display-group
      use: article-titlepage-verso-style ;; EVEN THOUGH IT'S RECTO!
      quadding: 'start
      start-indent: (+ (inherited-start-indent) (/ %body-width% 24))
      end-indent: (+ (inherited-end-indent) (/ %body-width% 24))
      ($semiformal-object$)))

  (element (abstract title) (empty-sosofo))

  (element address 
    (make display-group
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make display-group
      use: article-titlepage-recto-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element author
    (let ((author-name  (author-string))
	  (author-affil (select-elements (children (current-node)) 
					 (normalize "affiliation"))))
      (make sequence      
	(make paragraph
          use: article-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-before: (* (HSIZE 2) %head-before-factor%)
	  quadding: %article-title-quadding%
	  keep-with-next?: #t
	  (literal author-name))
	(process-node-list author-affil))))

  (element authorblurb
    (make display-group
      use: article-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element (authorblurb para)
    (make paragraph
      use: article-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element authorgroup
    (make display-group
      (process-children)))

  (element authorinitials
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element confgroup
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element contractnum
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element contrib
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element copyright
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    (make sequence
      (make paragraph
        use: article-titlepage-recto-style
	font-size: (HSIZE 3)
	line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	space-before: (* (HSIZE 2) %head-before-factor%)
	quadding: %article-title-quadding%
	keep-with-next?: #t
	(process-children))
      ;; This paragraph is a hack to get the spacing right.
      ;; Authors always have an affiliation paragraph below them, even if
      ;; it's empty, so corpauthors need one too.
      (make paragraph
        use: article-titlepage-recto-style
	font-size: (HSIZE 1)
	line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	space-after: (* (HSIZE 2) %head-after-factor% 4)
	quadding: %article-title-quadding%
	keep-with-next?: #t
	(literal "\no-break-space;"))))

  (element corpname
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element date
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element edition
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    (let ((editor-name (author-string)))
      (make sequence      
	(if (first-sibling?) 
	    (make paragraph
              use: article-titlepage-recto-style
	      font-size: (HSIZE 1)
	      line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	      space-before: (* (HSIZE 2) %head-before-factor% 6)
	      quadding: %article-title-quadding%
	      keep-with-next?: #t
	      (literal (gentext-edited-by)))
	    (empty-sosofo))
	(make paragraph
          use: article-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-after: (* (HSIZE 2) %head-after-factor% 4)
	  quadding: %article-title-quadding%
	  keep-with-next?: #t
	  (literal editor-name)))))

  (element firstname
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'center)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element isbn
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element issn
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element issuenum
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element jobtitle
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element keyword
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: article-titlepage-recto-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para)
    (make paragraph
      use: article-titlepage-recto-style
      quadding: 'start
      line-spacing: (* 0.8 (inherited-line-spacing))
      font-size: (* 0.8 (inherited-font-size))
      (process-children)))

  (element lineage
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element orgname
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element othercredit
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element othername
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element pagenums
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element printhistory
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element productname
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element productnumber
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element pubdate
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element publisher
    (make display-group
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element publishername
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: article-titlepage-recto-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: article-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: article-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: article-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: article-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo)))))))
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element shortaffil
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle 
    (make paragraph
      use: article-titlepage-recto-style
      font-size: (HSIZE 4)
      line-spacing: (* (HSIZE 4) %line-spacing-factor%)
      space-before: (* (HSIZE 4) %head-before-factor%)
      quadding: %article-subtitle-quadding%
      keep-with-next?: #t
      (process-children-trim)))

  (element surname
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))

  (element title 
    (make paragraph
      use: article-titlepage-recto-style
      font-size: (HSIZE 5)
      line-spacing: (* (HSIZE 5) %line-spacing-factor%)
      space-before: (* (HSIZE 5) %head-before-factor%)
      quadding: %article-title-quadding%
      keep-with-next?: #t
      (with-mode title-mode
	(process-children-trim))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: article-titlepage-recto-style
      quadding: %article-title-quadding%
      (process-children)))
)

(mode article-titlepage-verso-mode
  (element abbrev
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element abstract ($semiformal-object$))

  (element (abstract title) (empty-sosofo))

  (element address 
    (make display-group
      use: article-titlepage-verso-style
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element author
    ;; Print the author name.  Handle the case where there's no AUTHORGROUP
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (not in-group)
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-by))
	      (literal "\no-break-space;")
	      (literal (author-list-string))))
	  (make sequence 
	    (literal (author-list-string))))))  

  (element authorblurb
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element authorgroup
    (make paragraph
      space-after: (* %bf-size% %line-spacing-factor%)
      (make sequence
	(literal (gentext-by))
	(literal "\no-break-space;")
	(process-children-trim))))

  (element authorinitials
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element confgroup
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element contractnum
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element contrib
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element copyright
    (make paragraph
      use: article-titlepage-verso-style
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    ;; note: article-titlepage-corpauthor takes care of wrapping multiple
    ;; corpauthors
    (make sequence
      (if (first-sibling?)
	  (if (equal? (gi (parent (current-node))) (normalize "authorgroup"))
	      (empty-sosofo)
	      (literal (gentext-by) " "))
	  (literal ", "))
      (process-children)))

  (element corpname
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element date
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element edition
    (make paragraph
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    ;; Print the editor name.
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (or #t (not in-group)) ; nevermind, always put out the Edited by
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-edited-by))
	      (literal "\no-break-space;")
	      (literal (author-string))))
	  (make sequence 
	    (literal (author-string))))))

  (element firstname
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'start)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element isbn
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element issn
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element issuenum
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element jobtitle
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: article-titlepage-verso-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para) 
    (make paragraph
      use: article-titlepage-verso-style
      font-size: (* (inherited-font-size) 0.8)
      (process-children-trim)))

  (element lineage
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element orgname
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element othercredit
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element othername
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element pagenums
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element printhistory
    (make display-group
      use: article-titlepage-verso-style
      (process-children)))

  (element productname
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element productnumber
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element pubdate
    (make paragraph
      (literal (gentext-element-name-space (gi (current-node))))
      (process-children)))

  (element publisher
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element publishername
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: article-titlepage-verso-style
	font-family-name: %title-font-family%
	font-weight: 'bold
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (node-list-empty? revnumber)
		(empty-sosofo)
		(make paragraph
		  use: article-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (node-list-empty? revdate)
		(empty-sosofo)
		(make paragraph
		  use: article-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (node-list-empty? revauthor)
		(empty-sosofo)
		(make paragraph
		  use: article-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor)))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: article-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo))))))) 
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element shortaffil
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (literal (if (first-sibling?) ": " "; "))
      (process-children)))

  (element surname
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

  (element title
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (with-mode title-mode
	(process-children))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: article-titlepage-verso-style
      (process-children)))

)

;; == Title pages for REFERENCEs ========================================

(define (reference-titlepage-recto-elements)
  (list (normalize "title") 
	(normalize "subtitle")
	(normalize "corpauthor") 
	(normalize "authorgroup") 
	(normalize "author") 
	(normalize "editor")))

(define (reference-titlepage-verso-elements)
  '())

(define (reference-titlepage-content? elements side)
  (titlepage-content? elements (if (equal? side 'recto)
				   (reference-titlepage-recto-elements)
				   (reference-titlepage-verso-elements))))

(define (reference-titlepage elements #!optional (side 'recto))
  (let ((nodelist (titlepage-nodelist 
		   (if (equal? side 'recto)
		       (reference-titlepage-recto-elements)
		       (reference-titlepage-verso-elements))
		   elements))
        ;; partintro is a special case...
	(partintro (node-list-first
		    (node-list-filter-by-gi elements (list (normalize "partintro"))))))
    (if (reference-titlepage-content? elements side)
	(make simple-page-sequence
	  page-n-columns: %titlepage-n-columns%
	  input-whitespace-treatment: 'collapse
	  use: default-text-style
	  
	  ;; This hack is required for the RTF backend. If an external-graphic
	  ;; is the first thing on the page, RTF doesn't seem to do the right
	  ;; thing (the graphic winds up on the baseline of the first line
	  ;; of the page, left justified).  This "one point rule" fixes
	  ;; that problem.
	  (make paragraph
	    line-spacing: 1pt
	    (literal ""))
      
	  (let loop ((nl nodelist) (lastnode (empty-node-list)))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(make sequence
		  (if (or (node-list-empty? lastnode)
			  (not (equal? (gi (node-list-first nl))
				       (gi lastnode))))
		      (reference-titlepage-before (node-list-first nl) side)
		      (empty-sosofo))
		  (cond
		   ((equal? (gi (node-list-first nl)) (normalize "abbrev"))
		    (reference-titlepage-abbrev (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "abstract"))
		    (reference-titlepage-abstract (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "address"))
		    (reference-titlepage-address (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "affiliation"))
		    (reference-titlepage-affiliation (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "artpagenums"))
		    (reference-titlepage-artpagenums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "author"))
		    (reference-titlepage-author (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorblurb"))
		    (reference-titlepage-authorblurb (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorgroup"))
		    (reference-titlepage-authorgroup (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "authorinitials"))
		    (reference-titlepage-authorinitials (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "bibliomisc"))
		    (reference-titlepage-bibliomisc (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "biblioset"))
		    (reference-titlepage-biblioset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "bookbiblio"))
		    (reference-titlepage-bookbiblio (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "citetitle"))
		    (reference-titlepage-citetitle (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "collab"))
		    (reference-titlepage-collab (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "confgroup"))
		    (reference-titlepage-confgroup (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contractnum"))
		    (reference-titlepage-contractnum (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contractsponsor"))
		    (reference-titlepage-contractsponsor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "contrib"))
		    (reference-titlepage-contrib (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "copyright"))
		    (reference-titlepage-copyright (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "corpauthor"))
		    (reference-titlepage-corpauthor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "corpname"))
		    (reference-titlepage-corpname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "date"))
		    (reference-titlepage-date (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "edition"))
		    (reference-titlepage-edition (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "editor"))
		    (reference-titlepage-editor (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "firstname"))
		    (reference-titlepage-firstname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "graphic"))
		    (reference-titlepage-graphic (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "honorific"))
		    (reference-titlepage-honorific (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "indexterm"))
		    (reference-titlepage-indexterm (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "invpartnumber"))
		    (reference-titlepage-invpartnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "isbn"))
		    (reference-titlepage-isbn (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "issn"))
		    (reference-titlepage-issn (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "issuenum"))
		    (reference-titlepage-issuenum (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "itermset"))
		    (reference-titlepage-itermset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "keywordset"))
		    (reference-titlepage-keywordset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "legalnotice"))
		    (reference-titlepage-legalnotice (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "lineage"))
		    (reference-titlepage-lineage (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "mediaobject"))
		    (reference-titlepage-mediaobject (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "modespec"))
		    (reference-titlepage-modespec (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "orgname"))
		    (reference-titlepage-orgname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "othercredit"))
		    (reference-titlepage-othercredit (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "othername"))
		    (reference-titlepage-othername (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pagenums"))
		    (reference-titlepage-pagenums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "printhistory"))
		    (reference-titlepage-printhistory (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "productname"))
		    (reference-titlepage-productname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "productnumber"))
		    (reference-titlepage-productnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pubdate"))
		    (reference-titlepage-pubdate (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "publisher"))
		    (reference-titlepage-publisher (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "publishername"))
		    (reference-titlepage-publishername (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "pubsnumber"))
		    (reference-titlepage-pubsnumber (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "releaseinfo"))
		    (reference-titlepage-releaseinfo (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "revhistory"))
		    (reference-titlepage-revhistory (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "seriesinfo"))
		    (reference-titlepage-seriesinfo (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "seriesvolnums"))
		    (reference-titlepage-seriesvolnums (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "subjectset"))
		    (reference-titlepage-subjectset (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "subtitle"))
		    (reference-titlepage-subtitle (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "surname"))
		    (reference-titlepage-surname (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "title"))
		    (reference-titlepage-title (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "titleabbrev"))
		    (reference-titlepage-titleabbrev (node-list-first nl) side))
		   ((equal? (gi (node-list-first nl)) (normalize "volumenum"))
		    (reference-titlepage-volumenum (node-list-first nl) side))
		   (else
		    (reference-titlepage-default (node-list-first nl) side)))
		  (loop (node-list-rest nl) (node-list-first nl)))))

	  (if (and %generate-reference-toc%
		   %generate-reference-toc-on-titlepage%
		   (equal? side 'recto))
	      (make display-group
		(build-toc (current-node)
			   (toc-depth (current-node))))
	      (empty-sosofo))

	  ;; PartIntro is a special case
	  (if (and (equal? side 'recto)
		   (not (node-list-empty? partintro))
		   %generate-partintro-on-titlepage%)
	      ($process-partintro$ partintro #f)
	      (empty-sosofo)))

	(empty-sosofo))))

(define (reference-titlepage-before node side)
  (if (equal? side 'recto)
      (cond
       ((equal? (gi node) (normalize "corpauthor"))
	(make paragraph
	  space-after: (* (HSIZE 5) %head-after-factor% 8)
	  (literal "\no-break-space;")))
       ((equal? (gi node) (normalize "authorgroup"))
	(if (have-sibling? (normalize "corpauthor") node)
	    (empty-sosofo)
	    (make paragraph
	      space-after: (* (HSIZE 5) %head-after-factor% 8)
	      (literal "\no-break-space;"))))
       ((equal? (gi node) (normalize "author"))
	(if (or (have-sibling? (normalize "corpauthor") node) 
		(have-sibling? (normalize "authorgroup") node))
	    (empty-sosofo)
	    (make paragraph
	      space-after: (* (HSIZE 5) %head-after-factor% 8)
	      (literal "\no-break-space;"))))
       (else (empty-sosofo)))
      (empty-sosofo)))

(define (reference-titlepage-default node side)
  (let ((foo (debug (string-append "No reference-titlepage-* for " (gi node) "!"))))
    (empty-sosofo)))

(define (reference-titlepage-element node side)
  (if (equal? side 'recto)
      (with-mode reference-titlepage-recto-mode
	(process-node-list node))
      (with-mode reference-titlepage-verso-mode
	(process-node-list node))))

(define (reference-titlepage-abbrev node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-abstract node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-address node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-affiliation node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-artpagenums node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-author node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-authorblurb node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-authorgroup node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-authorinitials node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-bibliomisc node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-biblioset node side)
  (reference-titlepage node side))
(define (reference-titlepage-bookbiblio node side)
  (reference-titlepage node side))
(define (reference-titlepage-citetitle node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-collab node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-confgroup node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-contractnum node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-contractsponsor node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-contrib  node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-copyright node side)
  (reference-titlepage-element node side))

(define (reference-titlepage-corpauthor node side)
  (if (equal? side 'recto)
      (book-titlepage-element node side)
      (if (first-sibling? node)
	  (make paragraph
	    (with-mode book-titlepage-verso-mode
	      (process-node-list
	       (select-elements (children (parent node)) 
				(normalize "corpauthor")))))
	  (empty-sosofo))))

(define (reference-titlepage-corpname node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-date node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-edition node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-editor node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-firstname node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-graphic node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-honorific node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-indexterm node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-invpartnumber node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-isbn node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-issn node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-issuenum node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-itermset node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-keywordset node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-legalnotice node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-lineage node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-mediaobject node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-modespec node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-orgname node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-othercredit node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-othername node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-pagenums node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-printhistory node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-productname node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-productnumber node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-pubdate node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-publisher node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-publishername node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-pubsnumber node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-releaseinfo node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-revhistory node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-seriesinfo node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-seriesvolnums node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-subjectset node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-subtitle node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-surname node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-title node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-titleabbrev node side)
  (reference-titlepage-element node side))
(define (reference-titlepage-volumenum node side)
  (reference-titlepage-element node side))

(define reference-titlepage-recto-style
  (style
      font-family-name: %title-font-family%
      font-weight: 'bold
      font-size: (HSIZE 1)))

(define reference-titlepage-verso-style
  (style
      font-family-name: %body-font-family%))

(mode reference-titlepage-recto-mode
  (element abbrev
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element abstract
    (make display-group
      use: reference-titlepage-recto-style
      quadding: 'start
      ($semiformal-object$)))

  (element (abstract title) (empty-sosofo))

  (element (abstract para)
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element address 
    (make display-group
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make display-group
      use: reference-titlepage-recto-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element author
    (let ((author-name  (author-string))
	  (author-affil (select-elements (children (current-node)) 
					 (normalize "affiliation"))))
      (make sequence      
	(make paragraph
	  use: reference-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-before: (* (HSIZE 2) %head-before-factor%)
	  quadding: %division-title-quadding%
	  keep-with-next?: #t
	  (literal author-name))
	(process-node-list author-affil))))

  (element authorblurb
    (make display-group
      use: reference-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element (authorblurb para)
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: 'start
      (process-children)))

  (element authorgroup
    (make display-group
      (process-children)))

  (element authorinitials
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element confgroup
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contractnum
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element contrib
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element copyright
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    (make sequence
      (make paragraph
        use: reference-titlepage-recto-style
	font-size: (HSIZE 3)
	line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	space-before: (* (HSIZE 2) %head-before-factor%)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	(process-children))
      ;; This paragraph is a hack to get the spacing right.
      ;; Authors always have an affiliation paragraph below them, even if
      ;; it's empty, so corpauthors need one too.
      (make paragraph
        use: reference-titlepage-recto-style
	font-size: (HSIZE 1)
	line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	space-after: (* (HSIZE 2) %head-after-factor% 4)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	(literal "\no-break-space;"))))

  (element corpname
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element date
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element edition
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    (let ((editor-name (author-string)))
      (make sequence      
	(if (first-sibling?) 
	    (make paragraph
              use: reference-titlepage-recto-style
	      font-size: (HSIZE 1)
	      line-spacing: (* (HSIZE 1) %line-spacing-factor%)
	      space-before: (* (HSIZE 2) %head-before-factor% 6)
	      quadding: %division-title-quadding%
	      keep-with-next?: #t
	      (literal (gentext-edited-by)))
	    (empty-sosofo))
	(make paragraph
          use: reference-titlepage-recto-style
	  font-size: (HSIZE 3)
	  line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	  space-after: (* (HSIZE 2) %head-after-factor% 4)
	  quadding: %division-title-quadding%
	  keep-with-next?: #t
	  (literal editor-name)))))

  (element firstname
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'center)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element isbn
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element issn
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element issuenum
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element jobtitle
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: reference-titlepage-recto-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para)
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: 'start
      line-spacing: (* 0.8 (inherited-line-spacing))
      font-size: (* 0.8 (inherited-font-size))
      (process-children)))

  (element lineage
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element orgname
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element othercredit
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element othername
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pagenums
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element printhistory
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element productname
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element productnumber
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pubdate
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element publisher
    (make display-group
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element publishername
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: reference-titlepage-recto-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: reference-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: reference-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: reference-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: reference-titlepage-recto-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo))))))) 
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element shortaffil
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle 
    (make paragraph
      use: reference-titlepage-recto-style
      font-size: (HSIZE 4)
      line-spacing: (* (HSIZE 4) %line-spacing-factor%)
      space-before: (* (HSIZE 4) %head-before-factor%)
      quadding: %division-subtitle-quadding%
      keep-with-next?: #t
      (process-children-trim)))

  (element surname
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))

  (element title
    (let ((ref (ancestor-member (current-node)
				(list (normalize "reference")))))
      (make paragraph
        use: reference-titlepage-recto-style
	font-size: (HSIZE 5)
	line-spacing: (* (HSIZE 5) %line-spacing-factor%)
	space-before: (* (HSIZE 5) %head-before-factor%)
	quadding: %division-title-quadding%
	keep-with-next?: #t
	heading-level: (if %generate-heading-level% 1 0)
	(literal (element-label ref)
		 (gentext-label-title-sep (gi ref)))
	(with-mode title-mode
	  (process-children)))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: reference-titlepage-recto-style
      quadding: %division-title-quadding%
      (process-children)))
)

(mode reference-titlepage-verso-mode
  (element abbrev
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element abstract ($semiformal-object$))

  (element (abstract title) (empty-sosofo))

  (element address 
    (make display-group
      use: reference-titlepage-verso-style
      (with-mode titlepage-address-mode 
	($linespecific-display$ %indent-address-lines% %number-address-lines%))))

  (element affiliation
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element artpagenums
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element author
    ;; Print the author name.  Handle the case where there's no AUTHORGROUP
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (not in-group)
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-by))
	      (literal "\no-break-space;")
	      (literal (author-list-string))))
	  (make sequence 
	    (literal (author-list-string))))))  

  (element authorblurb
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element authorgroup
    (make paragraph
      space-after: (* %bf-size% %line-spacing-factor%)
      (make sequence
	(literal (gentext-by))
	(literal "\no-break-space;")
	(process-children-trim))))

  (element authorinitials
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element bibliomisc (process-children))

  (element bibliomset (process-children))

  (element collab
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element confgroup
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element contractnum
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element contractsponsor
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element contrib
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element copyright
    (make paragraph
      use: reference-titlepage-verso-style
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (literal " "))))

  (element (copyright holder) ($charseq$))

  (element corpauthor
    ;; note: reference-titlepage-corpauthor takes care of wrapping multiple
    ;; corpauthors
    (make sequence
      (if (first-sibling?)
	  (if (equal? (gi (parent (current-node))) (normalize "authorgroup"))
	      (empty-sosofo)
	      (literal (gentext-by) " "))
	  (literal ", "))
      (process-children)))

  (element corpname
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element date
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element edition
    (make paragraph
      (process-children)
      (literal "\no-break-space;")
      (literal (gentext-element-name-space (gi (current-node))))))

  (element editor
    ;; Print the editor name.
    (let ((in-group (have-ancestor? (normalize "authorgroup") (current-node))))
      (if (or #t (not in-group)) ; nevermind, always put out the Edited by
	  (make paragraph
	    ;; Hack to get the spacing right below the author name line...
	    space-after: (* %bf-size% %line-spacing-factor%)
	    (make sequence
	      (literal (gentext-edited-by))
	      (literal "\no-break-space;")
	      (literal (author-string))))
	  (make sequence 
	    (literal (author-string))))))

  (element firstname
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element graphic
    (let* ((nd (current-node))
	   (fileref (attribute-string "fileref" nd))
	   (entityref (attribute-string "entityref" nd))
	   (format (attribute-string "format" nd))
	   (align (attribute-string "align" nd)))
      (if (or fileref entityref) 
	  (make external-graphic
	    notation-system-id: (if format format "")
	    entity-system-id: (if fileref 
				  (graphic-file fileref)
				  (if entityref 
				      (entity-generated-system-id entityref)
				      ""))
	    display?: #t
	    display-alignment: 'start)
	  (empty-sosofo))))

  (element honorific
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element isbn
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element issn
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element itermset (empty-sosofo))

  (element invpartnumber
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element issuenum
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element jobtitle
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element keywordset
    (make paragraph
      quadding: 'start
      (make sequence
	font-weight: 'bold
	(literal "Keywords: "))
      (process-children)))

  (element (keyword)
    (make sequence
      (process-children)
      (if (not (last-sibling?))
	  (literal ", ")
	  (literal ""))))

  (element legalnotice
    (make display-group
      use: reference-titlepage-verso-style
      ($semiformal-object$)))

  (element (legalnotice title) (empty-sosofo))

  (element (legalnotice para) 
    (make paragraph
      use: reference-titlepage-verso-style
      font-size: (* (inherited-font-size) 0.8)
      (process-children-trim)))

  (element lineage
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element modespec (empty-sosofo))

  (element orgdiv
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element orgname
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element othercredit
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element othername
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element pagenums
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element printhistory
    (make display-group
      use: reference-titlepage-verso-style
      (process-children)))

  (element productname
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element productnumber
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element pubdate
    (make paragraph
      (literal (gentext-element-name-space (gi (current-node))))
      (process-children)))

  (element publisher
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element publishername
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element pubsnumber
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element releaseinfo
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	use: reference-titlepage-verso-style
	space-before: (* (HSIZE 3) %head-before-factor%)
	space-after: (/ (* (HSIZE 1) %head-before-factor%) 2)
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(process-children))))
  
  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node)) 
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (select-elements (descendants (current-node))
				      (normalize "revremark"))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  use: reference-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revdate))
		(make paragraph
		  use: reference-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (process-node-list revdate))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    start-indent: 0pt
	    cell-before-column-margin: (if (equal? (print-backend) 'tex)
					   6pt
					   0pt)
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  use: reference-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  (literal (gentext-revised-by))
		  (process-node-list revauthor))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    start-indent: 0pt
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  use: reference-titlepage-verso-style
		  font-size: %bf-size%
		  font-weight: 'medium
		  space-after: (if (last-sibling?) 
				   0pt
				   (/ %block-sep% 2))
		  (process-node-list revremark))
		(empty-sosofo))))))) 
  
  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))

  (element seriesvolnums
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element shortaffil
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element subjectset (empty-sosofo))

  (element subtitle
    (make sequence
      font-family-name: %title-font-family%
      font-weight: 'bold
      (literal (if (first-sibling?) ": " "; "))
      (process-children)))

  (element surname
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

  (element title
    (let ((ref (ancestor-member (current-node)
				(list (normalize "reference")))))
      (make sequence
        font-family-name: %title-font-family%
        font-weight: 'bold
	(literal (element-label ref)
		 (gentext-label-title-sep (gi ref)))
	(with-mode title-mode
	  (process-children)))))

  (element formalpara ($para-container$))
  (element (formalpara title) ($runinhead$))
  (element (formalpara para) (make sequence (process-children)))

  (element titleabbrev (empty-sosofo))

  (element volumenum
    (make paragraph
      use: reference-titlepage-verso-style
      (process-children)))

)
