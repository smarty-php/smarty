;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================= LINKS AND ANCHORS ==========================

(element link
  ;; No warnings about missing targets.  Jade will do that for us, and
  ;; this way we can use -wno-idref if we really don't care.
  (let* ((endterm   (attribute-string (normalize "endterm")))
	 (linkend   (attribute-string (normalize "linkend")))
	 (target    (element-with-id linkend))
	 (etarget   (if endterm 
			(element-with-id endterm)
			(empty-node-list)))
	 (link-cont (if endterm
			(if (node-list-empty? etarget)
			    (literal 
			     (string-append "LINK CONTENT ENDTERM '"
					    endterm
					    "' MISSING"))
			    (with-mode xref-endterm-mode
			      (process-node-list etarget)))
			(process-children))))
    (if (node-list-empty? target)
	link-cont
	(make link 
	  destination: (node-list-address target)
	  link-cont))))

(element ulink 
  (make sequence
    (if (node-list-empty? (children (current-node)))
	(literal (attribute-string (normalize "url")))
	(make sequence
	  ($charseq$)
 	  (if (not (equal? (attribute-string (normalize "url"))
 			   (data-of (current-node))))
 	      (if %footnote-ulinks%
 		  (if (and (equal? (print-backend) 'tex) bop-footnotes)
 		      (make sequence
 			($ss-seq$ + (literal (footnote-number (current-node))))
 			(make page-footnote
 			  (make paragraph
 			    font-family-name: %body-font-family%
 			    font-size: (* %footnote-size-factor% %bf-size%)
 			    font-posture: 'upright
 			    quadding: %default-quadding%
 			    line-spacing: (* (* %footnote-size-factor% %bf-size%)
 					     %line-spacing-factor%)
 			    space-before: %para-sep%
 			    space-after: %para-sep%
 			    start-indent: %footnote-field-width%
 			    first-line-start-indent: (- %footnote-field-width%)
 			    (make line-field
 			      field-width: %footnote-field-width%
 			      (literal (footnote-number (current-node))
 				       (gentext-label-title-sep (normalize "footnote"))))
 			    (literal (attribute-string (normalize "url"))))))
 		      ($ss-seq$ + (literal (footnote-number (current-node)))))
 		  (if %show-ulinks%
 		      (make sequence
 			(literal " (")
 			(literal (attribute-string (normalize "url")))
 			(literal ")"))
 		      (empty-sosofo)))
 	      (empty-sosofo))))))

(element footnoteref 
  (process-element-with-id (attribute-string (normalize "linkend"))))

(element anchor
  ;; This is different than (empty-sosofo) alone because the backend
  ;; will hang an anchor off the empty sequence.
  (make sequence (empty-sosofo)))

(element beginpage (empty-sosofo))

;; ======================================================================

(define (olink-link)
  ;; This is an olink without a TARGETDOCENT, treat it as a link within
  ;; the same document.
  (let* ((localinfo (attribute-string (normalize "localinfo")))
	 (target    (element-with-id localinfo))
	 (linkmode  (attribute-string (normalize "linkmode")))
	 (modespec  (if linkmode (element-with-id linkmode) (empty-node-list)))
	 (xreflabel (if (node-list-empty? modespec)
			#f
			(attribute-string (normalize "xreflabel") modespec)))
	 (linktext  (strip (data-of (current-node)))))
    (if (node-list-empty? target)
	(make sequence
	  (error (string-append "OLink to missing ID '" localinfo "'"))
	  (if (and (equal? linktext "") xreflabel)
	      (literal xreflabel)
	      (process-children)))
	(if (equal? linktext "")
	    (if xreflabel
		(xref-general target xreflabel)
		(xref-general target))
	    (process-children)))))

(define (olink-simple)
  ;; Assumptions: 
  ;; - The TARGETDOCENT is identified by a public ID
  ;; - If the element has no content, the title extracted by
  ;;   (olink-resource-title) should be used
  ;; - The (olink-resource-title) function can deduce the title from
  ;;   the pubid and the sysid
  (let* ((target   (attribute-string (normalize "targetdocent")))
	 (pubid    (entity-public-id target))
	 (sysid    (system-id-filename target))
	 (title    (olink-resource-title pubid sysid))
	 (linktext (strip (data-of (current-node)))))
    (if (equal? linktext "")
	(make sequence
	  font-posture: 'italic
	  (literal title))
	(process-children))))

(define (olink-outline-xref olroot target linktext)
  (let* ((name  (attribute-string (normalize "name") target))
	 (label (attribute-string (normalize "label") target))
	 (title (select-elements (children target) (normalize "ttl")))
	 (substitute (list
		      (list "%g" (if name (literal name) (literal "")))
		      (list "%n" (if label (literal label) (literal "")))
		      (list "%t" (with-mode olink-title-mode
				   (process-node-list title)))))
	 (tlist   (match-split-list linktext (assoc-objs substitute))))
    (string-list-sosofo tlist substitute)))

(define (olink-outline)
  (let* ((target    (attribute-string (normalize "targetdocent")))
	 (linkmode  (attribute-string (normalize "linkmode")))
	 (localinfo (attribute-string (normalize "localinfo")))
	 (modespec  (if linkmode (element-with-id linkmode) (empty-node-list)))
	 (xreflabel (if (node-list-empty? modespec) 
			""
			(attribute-string (normalize "xreflabel") modespec)))
	 (pubid     (entity-public-id target))
	 (sysid     (system-id-filename target))
	 (basename  (trim-string sysid '(".sgm" ".xml" ".sgml")))
	 (olinkfile (string-append basename %olink-outline-ext%))
	 (olinkdoc  (sgml-parse olinkfile))
	 (olinkroot (node-property 'document-element olinkdoc))
	 (olnode    (if localinfo
			(element-with-id localinfo olinkroot)
			olinkroot))
	 (linktext (strip (data-of (current-node)))))
    (if (equal? linktext "")
	(olink-outline-xref olinkroot olnode xreflabel)
	(process-children))))

(element olink
  (if (not (attribute-string (normalize "targetdocent")))
      (olink-link)
      (if (attribute-string (normalize "linkmode"))
	  (olink-outline)
	  (olink-simple))))

(mode olink-title-mode
  (default (process-children))

  (element ttl
    (make sequence
      font-posture: 'italic
      (process-children)))

  (element it
    (make sequence
      font-posture: 'upright
      (process-children)))

  (element tt
    (make sequence
      font-family-name: %mono-font-family%
      (process-children)))

  (element sub 
    ($ss-seq$ -))

  (element sup 
    ($ss-seq$ +))
)

;; ======================================================================

(element xref
  (let* ((endterm (attribute-string (normalize "endterm")))
	 (linkend (attribute-string (normalize "linkend")))
	 (target  (element-with-id linkend))
	 (xreflabel (if (node-list-empty? target)
			#f
			(attribute-string (normalize "xreflabel") target))))
    (if (node-list-empty? target)
	(error (string-append "XRef LinkEnd to missing ID '" linkend "'"))
	(if xreflabel
	    (make link 
	      destination: (node-list-address target)
	      (literal xreflabel))
	    (if endterm
		(if (node-list-empty? (element-with-id endterm))
		    (error (string-append "XRef EndTerm to missing ID '" 
					  endterm "'"))
		    (make link 
		      destination: (node-list-address (element-with-id endterm))
		      (with-mode xref-endterm-mode 
			(process-element-with-id endterm))))
		(cond
		 ((or (equal? (gi target) (normalize "biblioentry"))
		      (equal? (gi target) (normalize "bibliomixed")))
		  ;; xref to the bibliography is a special case
		  (xref-biblioentry target))
		 ((equal? (gi target) (normalize "co"))
		  ;; callouts are a special case
		  (xref-callout target))
		 ((equal? (gi target) (normalize "listitem"))
		  (xref-listitem target))
		 ((equal? (gi target) (normalize "question"))
		  (xref-question target))
		 ((equal? (gi target) (normalize "answer"))
		  (xref-answer target))
		 ((equal? (gi target) (normalize "refentry"))
		  (xref-refentry target))
		 ((equal? (gi target) (normalize "refnamediv"))
		  ;; and refnamedivs
		  (xref-refnamediv target))
		 ((equal? (gi target) (normalize "glossentry"))
		  ;; as are glossentrys
		  (xref-glossentry target))
		 ((equal? (gi target) (normalize "author"))
		  ;; and authors
		  (xref-author target))
		 ((equal? (gi target) (normalize "authorgroup"))
		  ;; and authorgroups
		  (xref-authorgroup target))
		 (else 
		  (xref-general target))))))))

(define (xref-general target #!optional (xref-string #f))
  ;; This function is used by both XREF and OLINK (when no TARGETDOCENT
  ;; is specified).  The only case where xref-string is supplied is
  ;; on OLINK.
  (let ((label (attribute-string (normalize "xreflabel") target)))
    (make link 
      destination: (node-list-address target)
      (if xref-string
	  (auto-xref target xref-string)
	  (if label
	      (xreflabel-sosofo label)
	      (auto-xref target))))))

(define (xref-refentry target)
;; refmeta/refentrytitle, refmeta/manvolnum, refnamediv/refdescriptor, 
;; refnamediv/refname
  (let* ((refmeta    (select-elements (children target)
				      (normalize "refmeta")))
	 (refnamediv (select-elements (children target)
				      (normalize "refnamediv")))
	 (rfetitle   (select-elements (children refmeta) 
				      (normalize "refentrytitle")))
	 (manvolnum  (select-elements (children refmeta)
				      (normalize "manvolnum")))
	 (refdescrip (select-elements (children refnamediv)
				      (normalize "refdescriptor")))
	 (refname    (select-elements (children refnamediv)
				      (normalize "refname")))

	 (title      (if (node-list-empty? rfetitle)
			 (if (node-list-empty? refdescrip)
			     (node-list-first refname)
			     (node-list-first refdescrip))
			 (node-list-first rfetitle))))
  (make link 
    destination: (node-list-address target)

    (make sequence
      font-posture: (if %refentry-xref-italic% 
			'italic
			(inherited-font-posture))

      (process-node-list (children title))
      (if (and %refentry-xref-manvolnum%
	       (not (node-list-empty? manvolnum)))
	  (process-node-list manvolnum)
	  (empty-sosofo))))))

(define (xref-refnamediv target)
  (let* ((refname    (select-elements (children target)
				      (normalize "refname")))

	 (title      (node-list-first refname)))
    (make link
      destination: (node-list-address target)

      (make sequence
	font-posture: (if %refentry-xref-italic%
			  'italic
			  (inherited-font-posture))

	(process-node-list (children title))))))

(define (xref-glossentry target)
  (let ((glossterms (select-elements (children target)
				     (normalize "glossterm"))))
    (make link 
      destination: (node-list-address target)
      (with-mode xref-glossentry-mode
	(process-node-list (node-list-first glossterms))))))

(define (xref-author target)
  (make link 
    destination: (node-list-address target)
    (literal (author-string target))))

(define (xref-authorgroup target)
  ;; it's a quirk of author-list-string that it needs to point to
  ;; one of the authors in the authorgroup, not the authorgroup.
  ;; go figure.
  (make link 
    destination: (node-list-address target)
    (let loop ((author (select-elements (children target)
					(normalize "author"))))
      (if (node-list-empty? author)
	  (empty-sosofo)
	  (make sequence
	    (literal (author-list-string (node-list-first author)))
	    (loop (node-list-rest author)))))))

(define (xref-biblioentry target)
  (let* ((abbrev (node-list-first 
		  (node-list-filter-out-pis (children target))))
	 (label  (attribute-string (normalize "xreflabel") target)))
    (make link 
      destination: (node-list-address target)

      (if biblio-xref-title
	  (let* ((citetitles (select-elements (descendants target)
					     (normalize "citetitle")))
		 (titles     (select-elements (descendants target)
					     (normalize "title")))
		 (title      (if (node-list-empty? citetitles)
				 (node-list-first titles)
				 (node-list-first citetitles))))
	    (with-mode xref-title-mode
	      (process-node-list title)))
	  (if biblio-number
	      (make sequence
		(literal "[" (number->string (bibentry-number target)) "]"))
	      (if label
		  (make sequence
		    (literal "[" label "]"))
		  (if (equal? (gi abbrev) (normalize "abbrev"))
		      (make sequence
			(process-node-list abbrev))
		      (make sequence
			(literal "[" 
				 (attribute-string (normalize "id") target)
				 "]")))))))))

(define (xref-callout target)
  (make link 
    destination: (node-list-address target)
    ($callout-mark$ target)))

(define (xref-listitem target)
  (if (equal? (gi (parent target)) (normalize "orderedlist"))
      (make link 
	destination: (node-list-address target)
	(literal (orderedlist-listitem-label-recursive target)))
      (error 
       (string-append "XRef to LISTITEM only supported in ORDEREDLISTs"))))


(define (xref-question target)
  (make link
    destination: (node-list-address target)
    (make sequence
      (literal (gentext-element-name target))
      (literal (gentext-label-title-sep target))
      (literal (question-answer-label target)))))

(define (xref-answer target)
  (xref-question target))

(mode xref-endterm-mode
  (default
    (make sequence
      font-posture: 'italic
      (process-children-trim))))

(define (xreflabel-sosofo xreflabel)
  (make sequence
    font-posture: 'italic
    (literal xreflabel)))

;; ======================================================================

;; Returns the title of the element as a sosofo, italicized for xref.
;;
(define (element-title-xref-sosofo nd)
  (make sequence
    font-posture: 'italic
    (element-title-sosofo nd)))

(mode xref-title-mode
  (element title
    (make sequence
      font-posture: 'italic
      (process-children-trim)))

  (element citetitle
    (make sequence
      font-posture: 'italic
      (process-children-trim)))

  (element refname
    (process-children-trim))

  (element refentrytitle
    (process-children-trim))
)

(mode xref-glossentry-mode
  (element glossterm
    ($italic-seq$)))

;; ======================================================================

(define (element-page-number-sosofo target)
  (with-mode pageno-mode
	(process-node-list target)))

(mode pageno-mode
  (default
    (current-node-page-number-sosofo)))

;; ======================================================================

