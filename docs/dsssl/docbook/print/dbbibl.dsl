;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; ......................... BIBLIOGRAPHY PARAMS .........................

;; these should be in dbparam...
(define %biblsep% ", ")
(define %biblend% ".")

(define %biblioentry-in-entry-order% #t)

;; .................... BIBLIOGRAPHY and BIBLIODIV ......................

(define (bibliography-content)
  ;; Note that the code below works for both the case where the bibliography
  ;; has BIBLIODIVs and the case where it doesn't, by the slightly subtle
  ;; fact that if it does, then allentries will be (empty-node-list).
  (let* ((allbibcontent (children (current-node)))
	 (prebibcontent (node-list-filter-by-not-gi 
			 allbibcontent
			 (list (normalize "biblioentry")
			       (normalize "bibliomixed"))))
	 (allentries    (node-list-filter-by-gi 
			 allbibcontent
			 (list (normalize "biblioentry")
			       (normalize "bibliomixed"))))
	 (entries       (if biblio-filter-used
			    (biblio-filter allentries)
			    allentries)))
    (make sequence
      (process-node-list prebibcontent)
      (process-node-list entries))))

(element (book bibliography)
  (make simple-page-sequence
    page-n-columns: %page-n-columns%
    page-number-restart?: (or %page-number-restart% 
			      (book-start?) 
			      (first-chapter?))
    page-number-format: ($page-number-format$)
    use: default-text-style
    left-header:   ($left-header$)
    center-header: ($center-header$)
    right-header:  ($right-header$)
    left-footer:   ($left-footer$)
    center-footer: ($center-footer$)
    right-footer:  ($right-footer$)
    start-indent: %body-start-indent%
    input-whitespace-treatment: 'collapse
    quadding: %default-quadding%
    (make sequence
      ($component-title$)
      (bibliography-content))
    (make-endnotes)))

(element bibliography
  ;; A bibliography that's inside something else...
  (let* ((sect   (ancestor-member (current-node) 
				  (append (section-element-list)
					  (component-element-list))))
	 (hlevel (+ (SECTLEVEL sect) 1))
	 (hs     (HSIZE (- 4 hlevel))))
    (make sequence
      (make paragraph
	font-family-name: %title-font-family%
	font-weight:  (if (< hlevel 5) 'bold 'medium)
	font-posture: (if (< hlevel 5) 'upright 'italic)
	font-size: hs
	line-spacing: (* hs %line-spacing-factor%)
	space-before: (* hs %head-before-factor%)
	space-after:  (* hs %head-after-factor%)
	start-indent: (if (or (>= hlevel 3)
			      (member (gi) (list (normalize "refsect1") 
						 (normalize "refsect2") 
						 (normalize "refsect3"))))
			  %body-start-indent%
			  0pt)
	first-line-start-indent: 0pt
	quadding: %section-title-quadding%
	keep-with-next?: #t
	heading-level: (if %generate-heading-level% (+ hlevel 1) 0)
	(element-title-sosofo (current-node)))
      (bibliography-content))))

(element (bibliography title) (empty-sosofo))

(element bibliodiv
  (let* ((allentries (node-list-filter-by-gi (children (current-node))
					     (list (normalize "biblioentry")
						   (normalize "bibliomixed"))))
	 (entries (if biblio-filter-used
		      (biblio-filter allentries)
		      allentries)))
    (if (and biblio-filter-used (node-list-empty? entries))
	(empty-sosofo)
	(make display-group
	  space-before: %block-sep%
	  space-after: %block-sep%
	  start-indent: %body-start-indent%
	  (make sequence
	    ($section-title$)
	    (process-node-list entries))))))

(element (bibliodiv title) (empty-sosofo))

;; ..................... BIBLIOGRAPHY ENTRIES .........................

(define (biblioentry-inline-sep node rest)
  ;; Output the character that should separate inline node from rest
  (cond 
   ((and (equal? (gi node) (normalize "title"))
	 (equal? (gi (node-list-first rest)) (normalize "subtitle")))
    (make sequence
      font-posture: 'italic
      (literal ": ")))
   (else
    (literal %biblsep%))))

(define (biblioentry-inline-end blocks)
  ;; Output the character that should occur at the end of inline
  (literal %biblend%))

(define (biblioentry-block-sep node rest)
  ;; Output the character that should separate block node from rest
  (empty-sosofo))

(define (biblioentry-block-end)
  ;; Output the character that should occur at the end of block
  (empty-sosofo))

(element biblioentry
  (let* ((expanded-children   (expand-children 
			       (children (current-node))
			       (biblioentry-flatten-elements)))
	 (all-inline-children (if %biblioentry-in-entry-order%
				  (titlepage-gi-list-by-nodelist
				   (biblioentry-inline-elements)
				   expanded-children)
				  (titlepage-gi-list-by-elements
				   (biblioentry-inline-elements)
				   expanded-children)))
	 (block-children      (if %biblioentry-in-entry-order%
				  (titlepage-gi-list-by-nodelist
				   (biblioentry-block-elements)
				   expanded-children)
				  (titlepage-gi-list-by-elements
				   (biblioentry-block-elements)
				   expanded-children)))
	 (leading-abbrev      (if (equal? (normalize "abbrev")
					  (gi (node-list-first 
					       all-inline-children)))
				  (node-list-first all-inline-children)
				  (empty-node-list)))
	 (inline-children     (if (node-list-empty? leading-abbrev)
				  all-inline-children
				  (node-list-rest all-inline-children)))
	 (has-leading-abbrev? (not (node-list-empty? leading-abbrev)))
	 (xreflabel           (if (or has-leading-abbrev? biblio-number)
				  #f
				  (attribute-string (normalize "xreflabel")))))
    (make display-group
      (make paragraph
	space-before: %para-sep%
	space-after: %para-sep%
	start-indent: (+ (inherited-start-indent) 2pi)
	first-line-start-indent: -2pi

	(if (or biblio-number xreflabel has-leading-abbrev?)
	    (make sequence
	      (literal "[")

	      (if biblio-number 
		  (literal (number->string (bibentry-number (current-node))))
		  (empty-sosofo))
	
	      (if xreflabel
		  (literal xreflabel)
		  (empty-sosofo))
	
	      (if has-leading-abbrev?
		  (with-mode biblioentry-inline-mode 
		    (process-node-list leading-abbrev))
		  (empty-sosofo))

	      (literal "]\no-break-space;"))
	    (empty-sosofo))
	
	(let loop ((nl inline-children))
	  (if (node-list-empty? nl)
	      (empty-sosofo)
	      (make sequence
		(with-mode biblioentry-inline-mode
		  (process-node-list (node-list-first nl)))
		(if (node-list-empty? (node-list-rest nl))
		    (biblioentry-inline-end block-children)
		    (biblioentry-inline-sep (node-list-first nl)
					  (node-list-rest nl)))
		(loop (node-list-rest nl))))))

      (make display-group
	start-indent: (+ (inherited-start-indent) 2pi)
	(let loop ((nl block-children))
	  (if (node-list-empty? nl)
	      (empty-sosofo)
	      (make sequence
		(with-mode biblioentry-block-mode
		  (process-node-list (node-list-first nl)))
		(if (node-list-empty? (node-list-rest nl))
		    (biblioentry-block-end)
		    (biblioentry-block-sep (node-list-first nl)
					   (node-list-rest nl)))
		(loop (node-list-rest nl)))))))))

(mode biblioentry-inline-mode
  (element abbrev
    (make sequence
      (process-children)))

  (element affiliation
    (let ((inline-children (node-list-filter-by-not-gi 
			    (children (current-node))
			    (list (normalize "address")))))
      (let loop ((nl inline-children))
	(if (node-list-empty? nl)
	    (empty-sosofo)
	    (make sequence
	      (process-node-list (node-list-first nl))
	      (if (node-list-empty? (node-list-rest nl))
		  (empty-sosofo)
		  (literal ", "))
	      (loop (node-list-rest nl)))))))

  (element artpagenums
    (make sequence
      (process-children)))

  (element author
    (literal (author-list-string)))

  (element authorgroup
    (process-children))

  (element authorinitials
    (make sequence
      (process-children)))

  (element collab
    (let* ((nl (children (current-node)))
	   (collabname (node-list-first nl))
	   (affil (node-list-rest nl)))
      (make sequence
	(process-node-list collabname)
	(if (node-list-empty? affil)
	    (empty-sosofo)
	    (let loop ((nl affil))
	      (if (node-list-empty? nl)
		  (empty-sosofo)
		  (make sequence
		    (literal ", ")
		    (process-node-list (node-list-first nl))
		    (loop (node-list-rest nl)))))))))

  (element (collab collabname)
    (process-children))

  (element confgroup
    (let ((inline-children (node-list-filter-by-not-gi 
			    (children (current-node))
			    (list (normalize "address")))))
      (let loop ((nl inline-children))
	(if (node-list-empty? nl)
	    (empty-sosofo)
	    (make sequence
	      (process-node-list (node-list-first nl))
	      (if (node-list-empty? (node-list-rest nl))
		  (empty-sosofo)
		  (literal ", "))
	      (loop (node-list-rest nl)))))))

  (element contractnum
    (process-children))

  (element contractsponsor
    (process-children))

  (element contrib
    (process-children))

  (element copyright
    ;; Just print the year(s)
    (let ((years (select-elements (children (current-node))
				  (normalize "year"))))
      (process-node-list years)))

  (element (copyright year)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (empty-sosofo))))

  (element corpauthor
    (make sequence
      (process-children)))

  (element corpname
    (make sequence
      (process-children)))

  (element date
    (make sequence
      (process-children)))

  (element edition
    (make sequence
      (process-children)))

  (element editor
    (make sequence
      (literal (gentext-edited-by))
      (literal " ")
      (literal (author-list-string))))

  (element firstname
    (make sequence
      (process-children)))

  (element honorific
    (make sequence
      (process-children)))

  (element invpartnumber
    (make sequence
      (process-children)))

  (element isbn
    (make sequence
      (process-children)))

  (element issn
    (make sequence
      (process-children)))

  (element issuenum
    (make sequence
      (process-children)))

  (element lineage
    (make sequence
      (process-children)))

  (element orgname
    (make sequence
      (process-children)))

  (element othercredit
    (literal (author-list-string)))

  (element othername
    (make sequence
      (process-children)))

  (element pagenums
    (make sequence
      (process-children)))

  (element productname
    (make sequence
      ($charseq$)
; this is actually a problem since "trade" is the default value for
; the class attribute. we can put this back in in DocBook 5.0, when
; class becomes #IMPLIED
;      (if (equal? (attribute-string "class") (normalize "trade"))
;	  (literal "\trade-mark-sign;")
;	  (empty-sosofo))
      ))
  
  (element productnumber
    (make sequence
      (process-children)))

  (element pubdate
    (make sequence
      (process-children)))

  (element publisher
    (let ((pubname (select-elements (children (current-node))
				    (normalize "publishername")))
	  (cities  (select-elements (descendants (current-node))
				    (normalize "city"))))
    (make sequence
      (process-node-list pubname)
      (if (node-list-empty? cities)
	  (empty-sosofo)
	  (literal ", "))
      (process-node-list cities))))

  (element publishername
    (make sequence
      (process-children)))

  (element (publisher address city)
    (make sequence
      (process-children)
      (if (not (last-sibling? (current-node)))
	  (literal ", ")
	  (empty-sosofo))))

  (element pubsnumber
    (make sequence
      (process-children)))

  (element releaseinfo
    (make sequence
      (process-children)))

  (element seriesvolnums
    (make sequence
      (process-children)))

  (element subtitle
    (make sequence
      font-posture: 'italic
      (process-children)))

  (element surname
    (make sequence
      (process-children)))

  (element title
    (make sequence
      font-posture: 'italic
      (process-children)))

  (element titleabbrev
    (make sequence
      (process-children)))

  (element volumenum
    (make sequence
      (process-children)))

  (element (bibliomixed title) 
      (make sequence
	font-posture: 'italic
	(process-children)))

  
  (element (bibliomixed subtitle) 
    (make sequence
      font-posture: 'italic
      (process-children)))

  (element (biblioset title)
    (let ((rel (case-fold-up 
		(inherited-attribute-string (normalize "relation")))))
      (cond
       ((equal? rel "ARTICLE") (make sequence
				 (literal (gentext-start-quote))
				 (process-children)
				 (literal (gentext-end-quote))))
       (else (make sequence
	       font-posture: 'italic
	       (process-children))))))

  (element (bibliomset title)
    (let ((rel (case-fold-up 
		(inherited-attribute-string (normalize "relation")))))
      (cond
       ((equal? rel "ARTICLE") (make sequence
				 (literal (gentext-start-quote))
				 (process-children)
				 (literal (gentext-end-quote))))
       (else        (make sequence
		      font-posture: 'italic
		      (process-children))))))
)

(mode biblioentry-block-mode
  (element abstract
    (make display-group
      (process-children)))

  (element (abstract title)
    (make paragraph
      font-weight: 'bold
      (process-children)))

  (element address
    ($linespecific-display$ %indent-address-lines% %number-address-lines%))

  (element authorblurb
    (make display-group
      (process-children)))

  (element printhistory
    (make display-group
      (process-children)))

  (element revhistory
    (make sequence
      (make paragraph
	font-weight: 'bold
	(literal (gentext-element-name (current-node))))
      (make table
	before-row-border: #f
	(make table-column
	  column-number: 1
	  width: (/ (- %body-width% (inherited-start-indent)) 3))
	(make table-column
	  column-number: 2
	  width: (/ (- %body-width% (inherited-start-indent)) 3))
	(make table-column
	  column-number: 3
	  width: (/ (- %body-width% (inherited-start-indent)) 3))
	(process-children))))

  (element (revhistory revision)
    (let ((revnumber (select-elements (descendants (current-node)) 
				      (normalize "revnumber")))
	  (revdate   (select-elements (descendants (current-node))
				      (normalize "date")))
	  (revauthor (select-elements (descendants (current-node))
				      (normalize "authorinitials")))
	  (revremark (node-list-filter-by-gi
		      (descendants (current-node))
		      (list (normalize "revremark")
			    (normalize "revdescription")))))
      (make sequence
	(make table-row
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    (if (not (node-list-empty? revnumber))
		(make paragraph
		  (make sequence
		    (literal (gentext-element-name-space (current-node)))
		    (process-node-list revnumber)))
		(empty-sosofo)))
	  (make table-cell
	    column-number: 2
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    (if (not (node-list-empty? revdate))
	      (make paragraph
		(process-node-list revdate))
	      (empty-sosofo)))
	  (make table-cell
	    column-number: 3
	    n-columns-spanned: 1
	    n-rows-spanned: 1
	    (if (not (node-list-empty? revauthor))
		(make paragraph
		  (make sequence
		    (literal (gentext-revised-by))
		    (process-node-list revauthor)))
		(empty-sosofo))))
	(make table-row
	  cell-after-row-border: #f
	  (make table-cell
	    column-number: 1
	    n-columns-spanned: 3
	    n-rows-spanned: 1
	    (if (not (node-list-empty? revremark))
		(make paragraph
		  space-after: %block-sep%
		  (process-node-list revremark))
		(empty-sosofo)))))))

  (element (revision revnumber) (process-children-trim))
  (element (revision date) (process-children-trim))
  (element (revision authorinitials) (process-children-trim))
  (element (revision revremark) (process-children-trim))
  (element (revision revdescription) (process-children))

  (element seriesinfo
    ;; This is a nearly biblioentry recursively...
    (let* ((expanded-children   (expand-children 
				 (children (current-node))
				 (biblioentry-flatten-elements)))
	   (all-inline-children (if %biblioentry-in-entry-order%
				    (titlepage-gi-list-by-nodelist
				     (biblioentry-inline-elements)
				     expanded-children)
				    (titlepage-gi-list-by-elements
				     (biblioentry-inline-elements)
				     expanded-children)))
	   (block-children      (if %biblioentry-in-entry-order%
				    (titlepage-gi-list-by-nodelist
				     (biblioentry-block-elements)
				     expanded-children)
				    (titlepage-gi-list-by-elements
				     (biblioentry-block-elements)
				     expanded-children)))
	   (inline-children     all-inline-children))
      (make display-group
	(make paragraph
	  space-before: %para-sep%
	  space-after: %para-sep%
	  start-indent: (+ (inherited-start-indent) 2pi)
	  first-line-start-indent: -2pi
	  
	  (let loop ((nl inline-children))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(make sequence
		  (with-mode biblioentry-inline-mode
		    (process-node-list (node-list-first nl)))
		  (if (node-list-empty? (node-list-rest nl))
		      (biblioentry-inline-end block-children)
		      (biblioentry-inline-sep (node-list-first nl)
					      (node-list-rest nl)))
		  (loop (node-list-rest nl))))))
	
	(make display-group
	  start-indent: (+ (inherited-start-indent) 2pi)
	  (let loop ((nl block-children))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(make sequence
		  (with-mode biblioentry-block-mode
		    (process-node-list (node-list-first nl)))
		  (if (node-list-empty? (node-list-rest nl))
		      (biblioentry-block-end)
		      (biblioentry-block-sep (node-list-first nl)
					     (node-list-rest nl)))
		  (loop (node-list-rest nl)))))))))
)

(element bibliomixed 
  (let* ((all-inline-children (children (current-node)))
	 (leading-abbrev      (if (equal? (normalize "abbrev")
					  (gi (node-list-first 
					       all-inline-children)))
				  (node-list-first all-inline-children)
				  (empty-node-list)))
	 (inline-children     (if (node-list-empty? leading-abbrev)
				  all-inline-children
				  (node-list-rest all-inline-children)))
	 (has-leading-abbrev? (not (node-list-empty? leading-abbrev)))
	 (xreflabel           (if (or has-leading-abbrev? biblio-number)
				  #f
				  (attribute-string (normalize "xreflabel")))))
    (make paragraph
      space-before: %para-sep%
      space-after: %para-sep%
      start-indent: (+ (inherited-start-indent) 2pi)
      first-line-start-indent: -2pi

      (if (or biblio-number xreflabel has-leading-abbrev?)
	  (make sequence
	    (literal "[")

	    (if biblio-number 
		(literal (number->string (bibentry-number (current-node))))
		(empty-sosofo))
	    
	    (if xreflabel
		(literal xreflabel)
		(empty-sosofo))
	    
	    (if has-leading-abbrev?
		(with-mode biblioentry-inline-mode 
		  (process-node-list leading-abbrev))
		(empty-sosofo))
	    
	    (literal "]\no-break-space;"))
	  (empty-sosofo))

      (with-mode biblioentry-inline-mode
	(process-children)))))

;; ....................... BIBLIOGRAPHY ELEMENTS .......................

;; These are element construction rules for bibliography elements that 
;; may occur outside of a BIBLIOENTRY or BIBLIOMIXED.

(element bibliomisc (process-children))
(element bibliomset (process-children))
(element biblioset (process-children))
(element bookbiblio (process-children))

(element street ($charseq$))
(element pob ($charseq$))
(element postcode ($charseq$))
(element city ($charseq$))
(element state ($charseq$))
(element country ($charseq$))
(element phone ($charseq$))
(element fax ($charseq$))
(element otheraddr ($charseq$))
(element affiliation ($charseq$))
(element shortaffil ($charseq$))
(element jobtitle ($charseq$))
(element orgdiv ($charseq$))
(element artpagenums ($charseq$))

(element author
  (make sequence
    (literal (author-list-string))))

(element authorgroup (process-children))

(element collab (process-children))
(element collabname ($charseq$))
(element authorinitials ($charseq$))
(element confgroup (process-children))
(element confdates ($charseq$))
(element conftitle ($charseq$))
(element confnum ($charseq$))
(element confsponsor ($charseq$))
(element contractnum ($charseq$))
(element contractsponsor ($charseq$))

(element copyright
  (make paragraph
    (make sequence
      (literal (gentext-element-name (current-node)))
      (literal "\no-break-space;")
      (literal (dingbat "copyright"))
      (literal "\no-break-space;")
      (process-children-trim))))

(element year
  (make sequence
    (process-children)
    (if (not (last-sibling? (current-node)))
	(literal ", ")
	(literal " "))))

(element holder ($charseq$))

(element corpauthor
  (make sequence
    (literal (author-list-string))))

(element corpname ($charseq$))
(element date ($charseq$))
(element edition ($charseq$))
(element editor ($charseq$))
(element isbn ($charseq$))
(element issn ($charseq$))
(element invpartnumber ($charseq$))
(element issuenum ($charseq$))

(element legalnotice ($semiformal-object$))
(element (legalnotice title) (empty-sosofo))

(element modespec (empty-sosofo))

(element orgname ($charseq$))

(element othercredit
  (make sequence
    (literal (author-list-string))))

(element pagenums ($charseq$))
(element contrib ($charseq$))

(element firstname ($charseq$))
(element honorific ($charseq$))
(element lineage ($charseq$))
(element othername ($charseq$))
(element surname ($charseq$))

(element printhistory (empty-sosofo))

(element productname
  (make sequence
    ($charseq$)
; this is actually a problem since "trade" is the default value for
; the class attribute. we can put this back in in DocBook 5.0, when
; class becomes #IMPLIED
;    (if (equal? (attribute-string "class") (normalize "trade"))
;	(literal "\trade-mark-sign;")
;	(empty-sosofo))
))

(element productnumber ($charseq$))
(element pubdate ($charseq$))
(element publisher (process-children))
(element publishername ($charseq$))
(element pubsnumber ($charseq$))
(element releaseinfo (empty-sosofo))
(element revision ($charseq$))
(element revnumber ($charseq$))
(element revremark ($charseq$))
(element revdescription ($block-container$))
(element seriesvolnums ($charseq$))
(element volumenum ($charseq$))

;; The (element (bookinfo revhistory)) construction rule is in dbinfo.dsl
;; It calls $book-revhistory$...
(define ($book-revhistory$)
  (make sequence
    (make paragraph
      use: title-style
      font-family-name: %title-font-family%
      font-weight: 'bold
      space-before: (* (HSIZE 3) %head-before-factor%)
      space-after: (* (HSIZE 1) %head-before-factor%)
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
	(revremark (node-list-filter-by-gi
		    (descendants (current-node))
		    (list (normalize "revremark")
			  (normalize "revdescription")))))
    (make sequence
      (make table-row
	(make table-cell
	  column-number: 1
	  n-columns-spanned: 1
	  n-rows-spanned: 1
	  (if (not (node-list-empty? revnumber))
	      (make paragraph
		(make sequence
		  (literal (gentext-element-name-space (current-node)))
		  (process-node-list revnumber)))
	      (empty-sosofo)))
	(make table-cell
	  column-number: 2
	  n-columns-spanned: 1
	  n-rows-spanned: 1
	  (if (not (node-list-empty? revdate))
	      (make paragraph
		(process-node-list revdate))
	      (empty-sosofo)))
	(make table-cell
	  column-number: 3
	  n-columns-spanned: 1
	  n-rows-spanned: 1
	  (if (not (node-list-empty? revauthor))
	      (make paragraph
		(make sequence
		  (literal (gentext-revised-by))
		  (process-node-list revauthor)))
	      (empty-sosofo))))
      (make table-row
	cell-after-row-border: #f
	(make table-cell
	  column-number: 1
	  n-columns-spanned: 3
	  n-rows-spanned: 1
	  (if (not (node-list-empty? revremark))
	      (make paragraph
		space-after: %block-sep%
		(process-node-list revremark))
	      (empty-sosofo)))))))

(element (revision revnumber) (process-children-trim))
(element (revision date) (process-children-trim))
(element (revision authorinitials) (process-children-trim))
(element (revision revremark) (process-children-trim))
(element (revision revdescription) (process-children))
