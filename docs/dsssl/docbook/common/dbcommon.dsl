;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;
;; This file contains general functions common to both print and HTML
;; versions of the DocBook stylesheets.
;;

;; If **ANY** change is made to this file, you _MUST_ alter the
;; following definition:

(define %docbook-common-version%
  "Modular DocBook Stylesheet Common Functions")

;; === element lists ====================================================

;; these have to be functions because they have to be evaluated when
;; there is a current-node so that normalize can know what declaration
;; is in effect

(define (set-element-list)
  (list (normalize "set")))

(define (book-element-list)
  (list (normalize "book")))

(define (division-element-list)
  (list (normalize "part")))

(define (component-element-list)
  (list (normalize "preface")
	(normalize "chapter")
	(normalize "appendix") 
	(normalize "article")
	(normalize "glossary")
	(normalize "bibliography")
	(normalize "index")
	(normalize "colophon")
	(normalize "setindex")
	(normalize "reference")
	(normalize "refentry")
	(normalize "book"))) ;; just in case nothing else matches...

(define (major-component-element-list)
  (list (normalize "preface")
	(normalize "chapter") 
	(normalize "appendix") 
	(normalize "article")
	(normalize "glossary")
	(normalize "bibliography")
	(normalize "index")
	(normalize "colophon")
	(normalize "setindex")
	(normalize "reference")
	(normalize "refentry")
	(normalize "part")
	(normalize "book"))) ;; just in case nothing else matches...

(define (section-element-list)
  (list (normalize "sect1")
	(normalize "sect2")
	(normalize "sect3") 
	(normalize "sect4")
	(normalize "sect5")
	(normalize "section")
	(normalize "simplesect")
	(normalize "refsect1") 
	(normalize "refsect2") 
	(normalize "refsect3")))

(define (block-element-list)
  (list (normalize "example") 
	(normalize "figure") 
	(normalize "table") 
	(normalize "equation") 
	(normalize "procedure")))

(define (outer-parent-list)
  (list (normalize "toc") 
	(normalize "lot") 
	(normalize "appendix") 
	(normalize "chapter") 
	(normalize "part") 
	(normalize "preface") 
	(normalize "reference")
	(normalize "bibliography") 
	(normalize "glossary") 
	(normalize "index") 
	(normalize "setindex")
	(normalize "sect1") 
	(normalize "sect2") 
	(normalize "sect3") 
	(normalize "sect4") 
	(normalize "sect5") 
	(normalize "simplesect")
	(normalize "partintro") 
	(normalize "bibliodiv") 
	(normalize "glossdiv") 
	(normalize "indexdiv")
	(normalize "refentry") 
	(normalize "refsect1") 
	(normalize "refsect2") 
	(normalize "refsect3")
	(normalize "msgtext") 
	(normalize "msgexplan")))

(define (list-element-list)
  (list (normalize "orderedlist") 
	(normalize "itemizedlist") 
	(normalize "variablelist") 
	(normalize "segmentedlist")
        (normalize "simplelist") 
	(normalize "calloutlist") 
	(normalize "step")))

(define (info-element-list)
  (list (normalize "appendixinfo")
	(normalize "articleinfo")
	(normalize "bibliographyinfo")
	(normalize "bookinfo")
	(normalize "chapterinfo")
	(normalize "glossaryinfo")
	(normalize "indexinfo")
	(normalize "objectinfo")
	(normalize "partinfo")
	(normalize "prefaceinfo")
	(normalize "refentryinfo")
	(normalize "referenceinfo")
	(normalize "refsect1info")
	(normalize "refsect2info")
	(normalize "refsect3info")
	(normalize "refsynopsisdivinfo")
	(normalize "sect1info")
	(normalize "sect2info")
	(normalize "sect3info")
	(normalize "sect4info")
	(normalize "sect5info")
	(normalize "sectioninfo")
	(normalize "setindexinfo")
	(normalize "setinfo")
	(normalize "sidebarinfo")
	;; historical
	(normalize "artheader")
	(normalize "docinfo")))

;; === automatic TOC ====================================================

;; Returns #t if nd should appear in the auto TOC
(define (appears-in-auto-toc? nd)
  (if (or (equal? (gi nd) (normalize "refsect1"))
	  (have-ancestor? (normalize "refsect1") nd))
      #f
      #t))

;; # return elements of nl for which appears-in-auto-toc? is #t
(define (toc-list-filter nodelist)
  (let loop ((toclist (empty-node-list)) (nl nodelist))
    (if (node-list-empty? nl)
	toclist
	(if (appears-in-auto-toc? (node-list-first nl))
	    (loop (node-list toclist (node-list-first nl))
		  (node-list-rest nl))
	    (loop toclist (node-list-rest nl))))))
  
;; === common ===========================================================

(define (INLIST?)
  (has-ancestor-member? (current-node) (list-element-list)))

(define (INBLOCK?)
  (has-ancestor-member? (current-node) 
			(list (normalize "example") 
			      (normalize "informalexample")
			      (normalize "figure") 
			      (normalize "informalfigure")
			      (normalize "equation")
			      (normalize "informalequation")
			      (normalize "funcsynopsis")
			      (normalize "programlistingco")
			      (normalize "screenco")
			      (normalize "graphicco"))))

(define (PARNUM)
  (child-number (parent (current-node))))

(define (NESTEDFNUM n fmt)
  (if (number? n)
      (format-number n fmt)
      #f))

(define (FNUM n) (NESTEDFNUM n "1"))

(define (book-start?)
  ;; Returns #t if the current-node is in the first division or 
  ;; component of a book.
  (let ((book (ancestor (normalize "book")))
	(nd   (ancestor-member 
	       (current-node) 
	       (append (component-element-list) (division-element-list)))))
    (let loop ((ch (children book)))
      (if (node-list-empty? ch)
	  #f
	  (if (member (gi (node-list-first ch)) 
		      (append (component-element-list) (division-element-list)))
	      (node-list=? (node-list-first ch) nd)
	      (loop (node-list-rest ch)))))))

(define (first-chapter?)
  ;; Returns #t if the current-node is in the first chapter of a book
  (let* ((book (ancestor (normalize "book")))
	 (nd   (ancestor-member 
		(current-node) 
		(append (component-element-list) (division-element-list))))
	 (bookch (children book))
	 (bookcomp (expand-children bookch (list (normalize "part")))))
    (let loop ((nl bookcomp))
      (if (node-list-empty? nl)
	  #f
	  (if (equal? (gi (node-list-first nl)) (normalize "chapter"))
	      (if (node-list=? (node-list-first nl) nd)
		  #t
		  #f)
	      (loop (node-list-rest nl)))))))

;; === bibliographic ====================================================

;; Localized author-string

(define (author-list-string #!optional (author (current-node)))
  ;; Return a formatted string representation of the contents of AUTHOR
  ;; *including appropriate punctuation* if the AUTHOR occurs in a list
  ;; of AUTHORs in an AUTHORGROUP:
  ;;
  ;;   John Doe
  ;; or
  ;;   John Doe and Jane Doe
  ;; or
  ;;   John Doe, Jane Doe, and A. Nonymous
  ;;

  (let* ((author-node-list (select-elements
			    (descendants 
			     (ancestor (normalize "authorgroup") author))
			    (normalize "author")))
	 (corpauthor-node-list (select-elements
				(descendants 
				 (ancestor (normalize "authorgroup") author))
				(normalize "corpauthor")))
	 (othercredit-node-list (select-elements
				 (descendants 
				  (ancestor (normalize "authorgroup") author))
				 (normalize "othercredit")))
	 (editor-node-list (select-elements
			    (descendants 
			     (ancestor (normalize "authorgroup")))
			    (normalize "editor")))
	 (author-count (if (have-ancestor? (normalize "authorgroup") author)
			   (+ (node-list-length author-node-list)
			      (node-list-length corpauthor-node-list)
			      (node-list-length othercredit-node-list)
			      (node-list-length editor-node-list))
			   1))
	 (this-count (if (have-ancestor? (normalize "authorgroup") author)
			 (+ (node-list-length (preced author)) 1)
			 1)))
    (string-append
     (if (and (> author-count 1)
	      (last-sibling? author))
	 (string-append (gentext-and) " ")
	 "")

     (author-string author)

     (if (> author-count 2)
	 (if (> (- author-count this-count) 1)
	     (gentext-listcomma)
	     (if (= (- author-count this-count) 1)
		 (gentext-lastlistcomma)
		 ""))
	 "")
     (if (and (> author-count 1)
	      (not (last-sibling? author)))
	 " "
	 ""))))

;; === procedures =======================================================

(define ($proc-hierarch-number-format$ depth)
  (case (modulo depth 5)
    ((1) "1")
    ((2) "a")
    ((3) "i")
    ((4) "A")
    (else "I")))

(define ($proc-hierarch-number$ nd seperator)
  (if (equal? (gi nd) (normalize "step"))
      (string-append
       (format-number
	(child-number nd) 
	($proc-hierarch-number-format$ ($proc-step-depth$ nd)))
       seperator)
      ""))

(define ($proc-step-depth$ nd)
  (let loop ((step nd) (depth 0))
    (if (equal? (gi step) (normalize "procedure"))
	depth
	(loop (parent step) 
	      (if (equal? (gi step) (normalize "step"))
		  (+ depth 1)
		  depth)))))

(define ($proc-step-number$ nd)
  (let* ((step (if (equal? (gi nd) (normalize "step")) nd (parent nd)))
	 (str ($proc-hierarch-number$ step "")))
    (string-append str (gentext-label-title-sep (normalize "step")))))

(define ($proc-step-xref-number$ nd)
  (let loop ((step nd) (str "") (first #t))
    (if (equal? (gi step) (normalize "procedure"))
	str
	(loop (parent step) 
	      (if (equal? (gi step) (normalize "step"))
		  (string-append 
		   ($proc-hierarch-number$ step
				      (if first
					  ""
					  (gentext-intra-label-sep (normalize "step"))))
		   str)
		  str)
	      (if (equal? (gi step) (normalize "step"))
		  #f
		  first)))))

;; === sections =========================================================

(define (section-level-by-gi chunked? gi)
  ;; Figure out the heading level of an element by its name.  We need
  ;; to distinguish between the chunked processing mode (for HTML) and
  ;; the non-chunked (print or HTML).  It is important that no heading
  ;; level is skipped in a document structure (e.g., sect1 = 2, sect2
  ;; = 4); this results in broken PDF bookmarks.
  (if chunked?
      (cond
       ((equal? gi (normalize "sect5")) 5)
       ((equal? gi (normalize "sect4")) 4)
       ((equal? gi (normalize "sect3")) 3)
       ((equal? gi (normalize "sect2")) 2)
       ((equal? gi (normalize "sect1")) 1)
       ((equal? gi (normalize "refsect3")) 4)
       ((equal? gi (normalize "refsect2")) 3)
       ((equal? gi (normalize "refsect1")) 2)
       ((equal? gi (normalize "refsynopsisdiv")) 2)
       ((equal? gi (normalize "bibliography")) 1)
       ((equal? gi (normalize "bibliodiv")) 2)
       ((equal? gi (normalize "index")) 1)
       ((equal? gi (normalize "setindex")) 1)
       ((equal? gi (normalize "indexdiv")) 2)
       (else 1))
      (cond
       ((equal? gi (normalize "sect5")) 6)
       ((equal? gi (normalize "sect4")) 5)
       ((equal? gi (normalize "sect3")) 4)
       ((equal? gi (normalize "sect2")) 3)
       ((equal? gi (normalize "sect1")) 2)
       ;; The next four are not used by the HTML stylesheets.
       ((equal? gi (normalize "refsect3")) 5)
       ((equal? gi (normalize "refsect2")) 4)
       ((equal? gi (normalize "refsect1")) 3)
       ((equal? gi (normalize "refsynopsisdiv")) 3)
       ((equal? gi (normalize "bibliography")) 1)
       ((equal? gi (normalize "bibliodiv")) 2)
       ((equal? gi (normalize "index")) 1)
       ((equal? gi (normalize "setindex")) 1)
       ((equal? gi (normalize "indexdiv")) 2)
       (else 1))))

(define (section-level-by-node chunked? sect)
  (if (equal? (gi sect) (normalize "section"))
      ;; Section is special, it is recursive.
      (let ((depth (length (hierarchical-number-recursive 
			    (normalize "section")))))
	(if (> depth 5)
	    6
	    (+ depth 1)))
      (if (equal? (gi sect) (normalize "simplesect"))
	  ;; SimpleSect is special, it should be level "n+1", where "n" is
	  ;; the level of the numbered section that contains it.  If it is
	  ;; the *first* sectioning element in a chapter, make it 
	  ;; %default-simplesect-level%
          (cond
           ((have-ancestor? (normalize "sect5"))
	    (+ 1 (section-level-by-gi chunked? (normalize "sect5"))))
           ((have-ancestor? (normalize "sect4"))
	    (+ 1 (section-level-by-gi chunked? (normalize "sect4"))))
           ((have-ancestor? (normalize "sect3"))
	    (+ 1 (section-level-by-gi chunked? (normalize "sect3"))))
           ((have-ancestor? (normalize "sect2"))
	    (+ 1 (section-level-by-gi chunked? (normalize "sect2"))))
           ((have-ancestor? (normalize "sect1"))
	    (+ 1 (section-level-by-gi chunked? (normalize "sect1"))))
           ((have-ancestor? (normalize "refsect3"))
	    (+ 1 (section-level-by-gi chunked? (normalize "refsect3"))))
           ((have-ancestor? (normalize "refsect2"))
	    (+ 1 (section-level-by-gi chunked? (normalize "refsect2"))))
           ((have-ancestor? (normalize "refsect1"))
	    (+ 1 (section-level-by-gi chunked? (normalize "refsect1"))))
           (else %default-simplesect-level%))
	  ;; the rest of the section elements can be identified by name
	  (section-level-by-gi chunked? (gi sect)))))
  
;; === synopsis =========================================================

;; The following definitions match those given in the reference
;; documentation for DocBook V3.0
(define	%arg-choice-opt-open-str% "[")
(define	%arg-choice-opt-close-str% "]")
(define	%arg-choice-req-open-str% "{")
(define	%arg-choice-req-close-str% "}")
(define	%arg-choice-plain-open-str% " ")
(define	%arg-choice-plain-close-str% " ")
(define	%arg-choice-def-open-str% "[")
(define	%arg-choice-def-close-str% "]")
(define	%arg-rep-repeat-str% "...")
(define	%arg-rep-norepeat-str% "")
(define	%arg-rep-def-str% "")
(define %arg-or-sep% " | ")
(define %cmdsynopsis-hanging-indent% 4pi)

;; === linking ==========================================================

;; From the DocBook V3.0 Reference entry for element XREF:
;;
;; Description
;;
;;   Cross reference link to another part of the document. XRef is empty,
;;   and has common, Linkend, and Endterm attributes.
;;
;;   Processing Expectations
;;
;;   XRef must have a Linkend, but the Endterm is optional. If it is used,
;;   the content of the element it points to is displayed as the text of
;;   the cross reference; if it is absent, the XRefLabel of the
;;   cross-referenced object is displayed.
;;
;; If neither the ENDTERM nor the XREFLABEL is present, then the cross
;; reference text is taken from the (gentext-xref-strings) function
;; in the localization file, like this
;; 
;; A cross reference to an element, the target, begins with the
;; text returned by (gentext-xref-strings (gi target)).  Within
;; that text, the following substitutions are made:
;; 
;; %p is replaced by the number of the page on which target occurs
;; %g is replaced by the (gentext-element-name)
;; %n is replaced by the label
;; %t is replaced by the title
;;
;; After the "direct" cross reference, a number of indirect references
;; are possible.  If the target element is in a different block, section,
;; component, division, or book an indirect cross reference may be made.
;;
;; The indirect cross reference will only be made if
;;
;;   (auto-xref-indirect? target ancestor) 
;;
;; returns #t. The indirect reference is created by appending the
;; connect returned by (auto-xref-indirect-connector) to the direct
;; reference and then adding a direct refernce to the ancestor.
;; The process is repeated for each ancestral element.
;;
;; For example, if a direct reference to a section returns
;;
;;    "the section called %t"
;;
;; and a direct reference to a chapter returns 
;;
;;    "Chapter %n"
;;
;; and (auto-xref-indirect? sect1 chapter) returns #t, and 
;; (auto-xref-indirect-connector chapter) returns "in", then
;; an xref to a section in another chapter will be:
;;
;;    "the section called %t in Chapter %n"
;;
;; Where %t and %n will be filled in accordingly.
;;
;; ======================================================================

(define (auto-xref-indirect? target ancestor)
  ;; This function answers the question: should an indirect reference
  ;; to ancestor be made for target?  For example:
  ;;
  ;; (auto-xref-indirect? SECT1 CHAP)
  ;;
  ;; should return #t iff a reference of the form "in [CHAP-xref]" should
  ;; be generated for a reference to SECT1 if SECT1 is in a different
  ;; chapter than the XREF to SECT1.
  ;;
  ;; This function _does not_ have to consider the case of whether or
  ;; not target and the xref are in the same ancestor.  
  ;;
  (cond
   ;; Always add indirect references to another book
   ((member (gi ancestor) (book-element-list))
    #t)
   ;; Add indirect references to the section or component a block
   ;; is in iff chapters aren't autolabelled.  (Otherwise "Figure 1-3"
   ;; is sufficient)
   ((and (member (gi target) (block-element-list))
	 (not %chapter-autolabel%))
    #t)
   ;; Add indirect references to the component a section is in if
   ;; the sections are not autolabelled
   ((and (member (gi target) (section-element-list))
	 (member (gi ancestor) (component-element-list))
	 (not %section-autolabel%))
    #t)
   (else #f)))

(define (auto-xref-direct target 
			  #!optional 
			  (xref-string (gentext-xref-strings target)))
  (let* ((substitute (list
		      (list "%g"  (element-gi-sosofo target))
		      (list "%n"  (element-label-sosofo target))
		      (list "%p"  (element-page-number-sosofo target))
		      (list "%t"  (element-title-xref-sosofo target))))
	 (tlist   (match-split-list xref-string (assoc-objs substitute))))
    (string-list-sosofo tlist substitute)))

(define (auto-xref-indirect target 
			    #!optional
			    (xref-string (gentext-xref-strings target)))
  (make sequence
    (auto-xref-indirect-connector target)
    (auto-xref-direct target xref-string)))

(define (auto-xref target 
		   #!optional (xref-string (gentext-xref-strings target)))
  (let ((source (current-node))
	(cont-blok (ancestor-member target (block-element-list)))
	(cont-sect (ancestor-member target (section-element-list)))
	(cont-comp (ancestor-member target (component-element-list)))
	(cont-divn (ancestor-member target (division-element-list)))
	(cont-book (ancestor-member target (book-element-list))))
    (make sequence
      (auto-xref-direct target xref-string)
      (if (or (node-list=? cont-blok 
			   (ancestor-member source (block-element-list)))
	      (node-list=? cont-blok target)
	      (not (auto-xref-indirect? target cont-blok)))
	  (empty-sosofo)
	  (auto-xref-indirect cont-blok))
      (if (or (node-list=? cont-sect 
			   (ancestor-member source (section-element-list)))
	      (node-list=? cont-sect target)
	      (not (auto-xref-indirect? target cont-sect)))
	  (empty-sosofo)
	  (auto-xref-indirect cont-sect))
      (if (or (node-list=? cont-comp 
			   (ancestor-member source (component-element-list)))
	      (node-list=? cont-comp target)
	      (not (auto-xref-indirect? target cont-comp)))
	  (empty-sosofo)
	  (auto-xref-indirect cont-comp))
      (if (or (node-list=? cont-divn 
			   (ancestor-member source (division-element-list)))
	      (node-list=? cont-divn target)
	      (not (auto-xref-indirect? target cont-divn)))
	  (empty-sosofo)
	  (auto-xref-indirect cont-divn))
      (if (or (node-list=? cont-book 
			   (ancestor-member source (book-element-list)))
	      (node-list=? cont-book target)
	      (not (auto-xref-indirect? target cont-book)))
	  (empty-sosofo)
	  (auto-xref-indirect cont-book)))))

;; ======================================================================

(define (set-number-restart-list cmp)       (list (normalize "set")))
(define (book-number-restart-list cmp)      (list (normalize "set")))
(define (part-number-restart-list cmp)      (list (normalize "book")))
(define (reference-number-restart-list cmp) (list (normalize "book")))
(define (preface-number-restart-list cmp)   (list (normalize "book")))
(define (chapter-number-restart-list cmp)   (list (normalize "book")))
(define (appendix-number-restart-list cmp)  (list (normalize "book") 
						  (normalize "article")))
(define (article-number-restart-list cmp)   (list (normalize "book")))
(define (glossary-number-restart-list cmp)  (list (normalize "book")))
(define (bibliography-number-restart-list cmp) (list (normalize "book")))
(define (index-number-restart-list cmp)     (list (normalize "book")))
(define (setindex-number-restart-list cmp)  (list (normalize "set")))
(define (refentry-number-restart-list cmp)  (list (normalize "reference")))
(define (default-number-restart-list cmp)   (list (normalize "book")))

(define (component-number-restart-list cmp)
  ;; Return the list of elements at which numbering of 'cmp' should reset.
  ;; For example, for CHAPTER, it might return '("BOOK") causing chapters
  ;; to be sequentially numbered across a book.  If it returned
  ;; '("BOOK" "PART") then chapter numbering would restart at each 
  ;; BOOK or PART.
  (let ((name (gi cmp)))
    (cond
     ((equal? name (normalize "set"))       (set-number-restart-list cmp))
     ((equal? name (normalize "book"))      (book-number-restart-list cmp))
     ((equal? name (normalize "part"))      (part-number-restart-list cmp))
     ((equal? name (normalize "reference")) (reference-number-restart-list cmp))
     ((equal? name (normalize "preface"))   (preface-number-restart-list cmp))
     ((equal? name (normalize "chapter"))   (chapter-number-restart-list cmp))
     ((equal? name (normalize "appendix"))  (appendix-number-restart-list cmp))
     ((equal? name (normalize "article"))   (article-number-restart-list cmp))
     ((equal? name (normalize "glossary"))  (glossary-number-restart-list cmp))
     ((equal? name (normalize "bibliography")) (bibliography-number-restart-list cmp))
     ((equal? name (normalize "index"))     (index-number-restart-list cmp))
     ((equal? name (normalize "setindex"))  (setindex-number-restart-list cmp))
     ((equal? name (normalize "refentry"))  (refentry-number-restart-list cmp))
     (else (default-number-restart-list cmp)))))

(define (set-number-ignore-list cmp)       '())
(define (book-number-ignore-list cmp)      '())
(define (part-number-ignore-list cmp)      '())
(define (reference-number-ignore-list cmp) (list (normalize "part")))
(define (preface-number-ignore-list cmp)   (list (normalize "part")))
(define (chapter-number-ignore-list cmp)   (list (normalize "part")))
(define (appendix-number-ignore-list cmp)  (list (normalize "part")))
(define (article-number-ignore-list cmp)   (list (normalize "part")))
(define (glossary-number-ignore-list cmp)  (list (normalize "part")))
(define (bibliography-number-ignore-list cmp) (list (normalize "part")))
(define (index-number-ignore-list cmp)     (list (normalize "part")))
(define (setindex-number-ignore-list cmp)  (list (normalize "part")))
(define (refentry-number-ignore-list cmp)  '())
(define (default-number-ignore-list cmp)   '())

(define (component-number-ignore-list cmp)
  ;; Return the list of elements (inside the restart list) which are
  ;; hierarchy levels which should be ignored.  For example, for CHAPTER,
  ;; it might return '("PART") causing chapter numbering inside books
  ;; to ignore parts.
  ;;
  ;; Basically, if you skip up past a component/division element in
  ;; the restart list, you better put the element(s) you skipped in 
  ;; the ignore list or the stylesheet may never see your component
  ;; when it's trying to do the numbering.
  (let ((name (gi cmp)))
    (cond
     ((equal? name (normalize "set"))       (set-number-ignore-list cmp))
     ((equal? name (normalize "book"))      (book-number-ignore-list cmp))
     ((equal? name (normalize "part"))      (part-number-ignore-list cmp))
     ((equal? name (normalize "reference")) (reference-number-ignore-list cmp))
     ((equal? name (normalize "preface"))   (preface-number-ignore-list cmp))
     ((equal? name (normalize "chapter"))   (chapter-number-ignore-list cmp))
     ((equal? name (normalize "appendix"))  (appendix-number-ignore-list cmp))
     ((equal? name (normalize "article"))   (article-number-ignore-list cmp))
     ((equal? name (normalize "glossary"))  (glossary-number-ignore-list cmp))
     ((equal? name (normalize "bibliography")) (bibliography-number-ignore-list cmp))
     ((equal? name (normalize "index"))     (index-number-ignore-list cmp))
     ((equal? name (normalize "setindex"))  (setindex-number-ignore-list cmp))
     ((equal? name (normalize "refentry"))  (refentry-number-ignore-list cmp))
     (else (default-number-ignore-list cmp)))))

(define (set-number-sibling-list cmp)       '())
(define (book-number-sibling-list cmp)      '())
(define (part-number-sibling-list cmp)      '())
(define (reference-number-sibling-list cmp) '())
(define (preface-number-sibling-list cmp)   '())
(define (chapter-number-sibling-list cmp)   '())
(define (appendix-number-sibling-list cmp)  '())
(define (article-number-sibling-list cmp)   '())
(define (glossary-number-sibling-list cmp)  '())
(define (bibliography-number-sibling-list cmp) '())
(define (index-number-sibling-list cmp)     '())
(define (setindex-number-sibling-list cmp)  '())
(define (refentry-number-sibling-list cmp)  '())
(define (default-number-sibling-list cmp)   '())

(define (component-number-sibling-list cmp)
  ;; Return the list of elements with which 'cmp' should be numbered.
  ;; For example, for PART it might return '("PART" "REFERENCE") causing
  ;; sibling parts and references to be numbered together.
  (let ((name (gi cmp)))
    (cond
     ((equal? name (normalize "set"))       (set-number-sibling-list cmp))
     ((equal? name (normalize "book"))      (book-number-sibling-list cmp))
     ((equal? name (normalize "part"))      (part-number-sibling-list cmp))
     ((equal? name (normalize "reference")) (reference-number-sibling-list cmp))
     ((equal? name (normalize "preface"))   (preface-number-sibling-list cmp))
     ((equal? name (normalize "chapter"))   (chapter-number-sibling-list cmp))
     ((equal? name (normalize "appendix"))  (appendix-number-sibling-list cmp))
     ((equal? name (normalize "article"))   (article-number-sibling-list cmp))
     ((equal? name (normalize "glossary"))  (glossary-number-sibling-list cmp))
     ((equal? name (normalize "bibliography")) (bibliography-number-sibling-list cmp))
     ((equal? name (normalize "index"))     (index-number-sibling-list cmp))
     ((equal? name (normalize "setindex"))  (setindex-number-sibling-list cmp))
     ((equal? name (normalize "refentry"))  (refentry-number-sibling-list cmp))
     (else (default-number-sibling-list cmp)))))

(define (component-number component-node)
  (let* ((root  (ancestor-member component-node 
				 (component-number-restart-list 
				  component-node)))
	 (clist (expand-children (children root) 
				 (component-number-ignore-list
				  component-node)))
	 (slist (append (list (gi component-node))
			(component-number-sibling-list component-node))))
    (let loop ((nl clist) (count 1))
      (if (node-list-empty? nl) 
	  1
	  (if (node-list=? (node-list-first nl) component-node)
	      count
	      (if (member (gi (node-list-first nl)) slist)
		  (loop (node-list-rest nl) (+ count 1))
		  (loop (node-list-rest nl) count)))))))

;; == components and divisions == 

(define (set-autolabel nd #!optional (force-label? #f))
  "")

(define (book-autolabel nd #!optional (force-label? #f))
  "")

(define (part-autolabel nd #!optional (force-label? #f))
  (format-number (component-number nd) (label-number-format nd)))

(define (reference-autolabel nd #!optional (force-label? #f))
  (format-number (component-number nd) (label-number-format nd)))

(define (preface-autolabel nd #!optional (force-label? #f))
  "")

(define (chapter-autolabel nd #!optional (force-label? #f))
  (if (or force-label? %chapter-autolabel%)
      (format-number (component-number nd) (label-number-format nd))
      ""))

(define (appendix-autolabel nd #!optional (force-label? #f))
  ;; Abandoned special processing for appendixes in articles. Maybe
  ;; it's a good idea, but it can't be done here because it screws
  ;; up cross references to appendixes.
  (if (or force-label? %chapter-autolabel%)
      (format-number (component-number nd) (label-number-format nd))
      ""))

(define (article-autolabel nd #!optional (force-label? #f))
  "")

(define (glossary-autolabel nd #!optional (force-label? #f))
  "")

(define (bibliography-autolabel nd #!optional (force-label? #f))
  "")

(define (index-autolabel nd #!optional (force-label? #f))
  "")

(define (indexdiv-autolabel nd #!optional (force-label? #f))
  "")

(define (colophon-autolabel nd #!optional (force-label? #f))
  "")

(define (setindex-autolabel nd #!optional (force-label? #f))
  "")

(define (refentry-autolabel nd #!optional (force-label? #f))
  (let* ((isep       (gentext-intra-label-sep nd))
	 (refnamediv (select-elements (children nd)
				      (normalize "refnamediv")))
	 (refd       (select-elements (children refnamediv)
				      (normalize "refdescriptor")))
	 (refnames   (select-elements (children refnamediv)
				      (normalize "refname"))))
    ""))

;; == /components and divisions == 

(define (dedication-autolabel nd #!optional (force-label? #f))
  "")

(define (bibliodiv-autolabel nd #!optional (force-label? #f))
  "")

(define (glossdiv-autolabel nd #!optional (force-label? #f))
  "")

(define (section-autolabel-prefix nd)
  (let* ((isep   (gentext-intra-label-sep nd))
	 (haschn (not (node-list-empty? (ancestor (normalize "chapter") nd))))
	 (hasapn (not (node-list-empty? (ancestor (normalize "appendix") nd)))))
    (cond
     (haschn (string-append 
	      (element-label (ancestor (normalize "chapter") nd)) isep))
     (hasapn (string-append 
	      (element-label (ancestor (normalize "appendix") nd)) isep))
     (else ""))))

(define (section-autolabel nd #!optional (force-label? #f))
  (let* ((isep (gentext-intra-label-sep nd))
	 (hasprf (not (node-list-empty? (ancestor (normalize "preface") nd))))
	 (prefix (section-autolabel-prefix nd)))
    (if (and (or force-label? %section-autolabel%)
	     (or %label-preface-sections%
		 (not hasprf)))
	(cond
	 ((equal? (gi nd) (normalize "sect1"))
	  (string-append prefix (format-number (child-number nd) 
					       (label-number-format nd))))
	 ((equal? (gi nd) (normalize "sect2"))
	  (string-append 
	   (element-label (ancestor (normalize "sect1") nd) force-label?)
	   isep 
	   (format-number (child-number nd) (label-number-format nd))))
	 ((equal? (gi nd) (normalize "sect3"))
	  (string-append
	   (element-label (ancestor (normalize "sect2") nd) force-label?)
	   isep 
	   (format-number (child-number nd) (label-number-format nd))))
	 ((equal? (gi nd) (normalize "sect4"))
	  (string-append
	   (element-label (ancestor (normalize "sect3") nd) force-label?)
	   isep 
	   (format-number (child-number nd) (label-number-format nd))))
	 ((equal? (gi nd) (normalize "sect5"))
	  (string-append 
	   (element-label (ancestor (normalize "sect4") nd) force-label?)
	   isep 
	   (format-number (child-number nd) (label-number-format nd))))

	 ((equal? (gi nd) (normalize "simplesect"))
	  (let* ((possible-sect-ancestors
		  (node-list (ancestor (normalize "section") nd)
			     (ancestor (normalize "sect5") nd)
			     (ancestor (normalize "sect4") nd)
			     (ancestor (normalize "sect3") nd)
			     (ancestor (normalize "sect2") nd)
			     (ancestor (normalize "sect1") nd)))
		 (section-ancestor (node-list-first possible-sect-ancestors)))
	    (if (node-list-empty? section-ancestor)
		(string-append prefix (format-number (child-number nd) 
						     (label-number-format nd)))
		(string-append 
		 (element-label section-ancestor force-label?)
		 isep 
		 (format-number (child-number nd) (label-number-format nd))))))

	 ((equal? (gi nd) (normalize "section"))
	  (if (node-list-empty? (ancestor (normalize "section") nd))
	      (string-append prefix (format-number (child-number nd) 
						   (label-number-format nd)))
	      (string-append 
	       (element-label (ancestor (normalize "section") nd) force-label?)
	       isep 
	       (format-number (child-number nd) (label-number-format nd)))))
	 (else (string-append (gi nd) " IS NOT A SECTION!")))
	"")))
  
(define (refsection-autolabel nd #!optional (force-label? #f))
  "")

(define (step-autolabel nd #!optional (force-label? #f))
  ($proc-step-xref-number$ nd))

(define (listitem-autolabel nd #!optional (force-label? #f))
  (if (equal? (gi (parent nd)) (normalize "orderedlist"))
      (number->string (child-number nd))
      "[xref to LISTITEM only supported in ORDEREDLIST]"))

(define (sidebar-autolabel nd #!optional (force-label? #f))
  "")

(define (legalnotice-autolabel nd #!optional (force-label? #f))
  "")

(define (abstract-autolabel nd #!optional (force-label? #f))
  "")

(define (block-autolabel nd #!optional (force-label? #f))
  (let* ((chn    (element-label (ancestor (normalize "chapter") nd)))
	 (apn    (element-label (ancestor (normalize "appendix") nd)))
	 (rfn    (element-label (ancestor (normalize "refentry") nd)))
	 ;; If the root of this document isn't in component-element-list, these
	 ;; things all wind up being numbered 0. To avoid that, we force the
	 ;; root element to be in the list of components if it isn't already
	 ;; a component.
	 (incomp (member (gi (sgml-root-element)) (component-element-list)))
	 ;; In articles in books, number blocks from book not from article.
	 ;; Otherwise you get 1, 1, 1, 1, etc. for the first figure in each
	 ;; article.
	 (artinbook (and (not (node-list-empty? (ancestor (normalize "article") nd)))
			 (not (node-list-empty? (ancestor (normalize "book") nd)))))

	 (bkn    (if artinbook
		     (format-number (component-child-number
				     nd
				     (list (normalize "book")))
				    (label-number-format nd))
		     (if incomp
			 (format-number (component-child-number
					 nd
					 (component-element-list))
					(label-number-format nd))
			 (format-number (component-child-number
					 nd
					 (append (component-element-list)
						 (list (gi (sgml-root-element)))))
					(label-number-format nd))))))
    (if (equal? chn "")
	(if (equal? apn "")
	    (if (equal? rfn "")
		bkn
		(string-append rfn (gentext-intra-label-sep nd) bkn))
	    (string-append apn (gentext-intra-label-sep nd) bkn))
	(string-append chn (gentext-intra-label-sep nd) bkn))))

;; For all elements, if a LABEL attribute is present, that is the label
;; that they get.  Otherwise:
;; BOOK gets the Book volume, by book-autolabel
;; PREFACE gets "", by preface-autolabel
;; CHAPTER gets the Chapter number, by chapter-autolabel
;; APPENDIX gets the Appendix letter, by appendix-autolabel
;; REFERENCE gets "", by reference-autolabel
;; REFENTRY gets "", by refentry-autolabel
;; SECT* gets the nested section number (e.g., 1.3.5), by section-autolabel
;; REFSECT* gets the nested section number, by refsection-autolabel
;; everything else gets numbered by block-autolabel
;;
(define (element-label #!optional (nd (current-node)) (force-label? #f))
  (if (node-list-empty? nd)
      ""
      (let ((label (attribute-string (normalize "label") nd)))
	(if label
	    label
	    (cond
	     ;; Use a seperately defined assoc list?
	     ((equal? (gi nd) (normalize "abstract"))
	      (abstract-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "appendix"))
	      (appendix-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "article"))
	      (article-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "bibliodiv"))
	      (bibliodiv-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "bibliography"))
	      (bibliography-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "book"))
	      (book-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "chapter"))
	      (chapter-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "dedication"))
	      (dedication-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "glossary"))
	      (glossary-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "glossdiv"))
	      (glossdiv-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "index"))
	      (index-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "colophon"))
	      (colophon-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "indexdiv"))
	      (indexdiv-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "setindex"))
	      (setindex-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "legalnotice"))
	      (legalnotice-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "listitem"))
	      (listitem-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "part"))
	      (part-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "preface"))
	      (preface-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "refentry"))
	      (refentry-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "reference"))
	      (reference-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "refsynopsisdiv"))
	      (refsection-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "refsect1"))
	      (refsection-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "refsect2"))
	      (refsection-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "refsect3"))
	      (refsection-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "sect1"))
	      (section-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "sect2"))
	      (section-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "sect3"))
	      (section-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "sect4"))
	      (section-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "sect5"))
	      (section-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "section"))
	      (section-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "simplesect"))
	      (section-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "set"))
	      (set-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "sidebar"))
	      (sidebar-autolabel nd force-label?))
	     ((equal? (gi nd) (normalize "step"))
	      (step-autolabel nd force-label?))
	     (else (block-autolabel nd force-label?)))))))

;; ======================================================================

;; Returns the element label as a sosofo
;;
(define (element-label-sosofo nd #!optional (force-label? #f))
  (if (string=? (element-label nd force-label?) "")
      (empty-sosofo)
      (make sequence
	(literal (element-label nd force-label?)))))

;; ======================================================================

(define (set-title nd)
  (let* ((setinfo (select-elements (children nd) (normalize "setinfo")))
	 (sititles (select-elements  
		    (expand-children (children setinfo) 
				     (list (normalize "bookbiblio") 
					    (normalize "bibliomisc")
					    (normalize "biblioset")))
		    (normalize "title")))
	 (settitles (select-elements (children nd) (normalize "title")))
	 (titles   (if (node-list-empty? settitles)
		       sititles
		       settitles)))
    (if (node-list-empty? titles)
	""
	(node-list-first titles))))

(define (book-title nd)
  (let* ((bookinfo (select-elements (children nd) (normalize "bookinfo")))
	 (bititles (select-elements  
		    (expand-children (children bookinfo) 
				     (list (normalize "bookbiblio") 
					   (normalize "bibliomisc")
					   (normalize "biblioset")))
		    (normalize "title")))
	 (chtitles (select-elements (children nd) (normalize "title")))
	 (titles   (if (node-list-empty? chtitles)
		       bititles
		       chtitles)))
    (if (node-list-empty? titles)
	""
	(node-list-first titles))))

(define (part-title nd)
  (let* ((docinfo  (select-elements (children nd) (normalize "docinfo")))
	 (dititles (select-elements  
		    (expand-children (children docinfo) 
				     (list (normalize "bookbiblio") 
					   (normalize "bibliomisc")
					   (normalize "biblioset")))
		    (normalize "title")))
	 (chtitles (select-elements (children nd) (normalize "title")))
	 (titles   (if (node-list-empty? chtitles)
		       dititles
		       chtitles)))
    (if (node-list-empty? titles)
	""
	(node-list-first titles))))

(define (article-title nd)
  (let* ((artchild  (children nd))
	 (artheader (select-elements artchild (normalize "artheader")))
	 (ahtitles  (select-elements (children artheader) 
				     (normalize "title")))
	 (artitles  (select-elements artchild (normalize "title")))
	 (titles    (if (node-list-empty? artitles)
			ahtitles
			artitles)))
    (if (node-list-empty? titles)
	""
	(node-list-first titles))))

(define (preface-title nd)
  (chapter-title nd))

(define (chapter-title nd)
  (let* ((docinfo  (select-elements (children nd) (normalize "docinfo")))
	 (dititles (select-elements  
		    (expand-children (children docinfo) 
				     (list (normalize "bookbiblio") 
					   (normalize "bibliomisc")
					   (normalize "biblioset")))
		    (normalize "title")))
	 (chtitles (select-elements (children nd) (normalize "title")))
	 (titles   (if (node-list-empty? chtitles)
		       dititles
		       chtitles)))
    (if (node-list-empty? titles)
	""
	(node-list-first titles))))

(define (appendix-title nd)
  (chapter-title nd))

(define (reference-title nd)
  (chapter-title nd))

(define (refsynopsisdiv-title nd)
  (optional-title nd))

;; Returns either the REFENTRYTITLE or the first REFNAME.
;;
(define (refentry-title nd)
  (let* ((refmeta (select-elements (descendants nd) (normalize "refmeta")))
	 (refttl  (select-elements (descendants refmeta) (normalize "refentrytitle")))
	 (refndiv (select-elements (descendants nd) (normalize "refnamediv")))
	 (refname (select-elements (descendants refndiv) (normalize "refname"))))
    (if (node-list-empty? refttl)
	(if (node-list-empty? refname)
	    ""
	    (node-list-first refname))
	(node-list-first refttl))))

(define (optional-title nd)
  (let* ((docinfo  (select-elements (children nd) (normalize "docinfo")))
	 (dititles (select-elements (children docinfo) (normalize "title")))
	 (chtitles (select-elements (children nd) (normalize "title")))
	 (titles   (if (node-list-empty? chtitles)
		       dititles
		       chtitles)))
    (if (node-list-empty? titles)
	(gentext-element-name nd)
	(node-list-first titles))))

(define (glossary-title nd)
  (optional-title nd))

(define (bibliography-title nd)
  (optional-title nd))

(define (index-title nd)
  (optional-title nd))

(define (setindex-title nd)
  (optional-title nd))

(define (dedication-title nd)
  (optional-title nd))

(define (colophon-title nd)
  (gentext-element-name nd))

(define (section-title nd)
  (let* ((info     (select-elements (children nd) 
				    (list (normalize "sect1info")
					  (normalize "sect2info")
					  (normalize "sect3info")
					  (normalize "sect4info")
					  (normalize "sect5info")
					  (normalize "section"))))
	 (ititles  (select-elements (children info) (normalize "title")))
	 (ctitles  (select-elements (children nd) (normalize "title")))
	 (titles   (if (node-list-empty? ctitles)
		       ititles
		       ctitles)))
    (if (node-list-empty? titles)
	""
	(node-list-first titles))))

(define (refsection-title nd)
  (let* ((info     (select-elements (children nd) 
				    (list (normalize "refsect1info")
					  (normalize "refsect2info") 
					  (normalize "refsect3info"))))
	 (ititles  (select-elements (children info) (normalize "title")))
	 (ctitles  (select-elements (children nd) (normalize "title")))
	 (titles   (if (node-list-empty? ctitles)
		       ititles
		       ctitles)))
    (if (node-list-empty? titles)
	""
	(node-list-first titles))))

(define (block-title nd)
  (let ((titles (select-elements (children nd) (normalize "title"))))
    (if (node-list-empty? titles)
	""
	(node-list-first titles))))

;; ======================================================================

(define (set-title-sosofo nd)
  (let ((title (set-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (book-title-sosofo nd)
  (let ((title (book-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (part-title-sosofo nd)
  (let ((title (part-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (article-title-sosofo nd)
  (let ((title (article-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (preface-title-sosofo nd)
  (let ((title (preface-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (chapter-title-sosofo nd)
  (let ((title (chapter-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (appendix-title-sosofo nd)
  (let ((title (appendix-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (reference-title-sosofo nd)
  (let ((title (reference-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (refsynopsisdiv-title-sosofo nd)
  (optional-title-sosofo nd))

(define (refentry-title-sosofo nd)
  (let ((title (refentry-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (optional-title-sosofo nd)
  (let ((title (optional-title nd)))
    (if (string? title)
	(literal title)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (glossary-title-sosofo nd)
  (optional-title-sosofo nd))

(define (bibliography-title-sosofo nd)
  (optional-title-sosofo nd))

(define (index-title-sosofo nd)
  (optional-title-sosofo nd))

(define (setindex-title-sosofo nd)
  (optional-title-sosofo nd))

(define (dedication-title-sosofo nd)
  (optional-title-sosofo nd))

(define (colophon-title-sosofo nd)
  (literal (gentext-element-name nd)))

(define (section-title-sosofo nd)
  (let ((title (section-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (refsection-title-sosofo nd)
  (let ((title (refsection-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(define (block-title-sosofo nd)
  (let ((title (block-title nd)))
    (if (string? title)
	(empty-sosofo)
	(with-mode title-sosofo-mode
	  (process-node-list title)))))

(mode title-sosofo-mode
  (element title
    (process-children-trim))

  (element citetitle
    (process-children-trim))

  (element refname
    (process-children-trim))

  (element refentrytitle
    (process-children-trim)))

;; Returns the title of the element as a sosofo.
;;
(define (element-title-sosofo #!optional (nd (current-node)))
  (if (node-list-empty? nd)
      (empty-sosofo)
      (cond
       ;; Use a seperately defined assoc list?
       ((equal? (gi nd) (normalize "appendix")) (appendix-title-sosofo nd))
       ((equal? (gi nd) (normalize "article")) (article-title-sosofo nd))
       ((equal? (gi nd) (normalize "bibliography")) (bibliography-title-sosofo nd))
       ((equal? (gi nd) (normalize "book")) (book-title-sosofo nd))
       ((equal? (gi nd) (normalize "chapter")) (chapter-title-sosofo nd))
       ((equal? (gi nd) (normalize "dedication")) (dedication-title-sosofo nd))
       ((equal? (gi nd) (normalize "glossary")) (glossary-title-sosofo nd))
       ((equal? (gi nd) (normalize "index")) (index-title-sosofo nd))
       ((equal? (gi nd) (normalize "colophon")) (colophon-title-sosofo nd))
       ((equal? (gi nd) (normalize "setindex")) (index-title-sosofo nd))
       ((equal? (gi nd) (normalize "part")) (part-title-sosofo nd))
       ((equal? (gi nd) (normalize "preface")) (preface-title-sosofo nd))
       ((equal? (gi nd) (normalize "refentry")) (refentry-title-sosofo nd))
       ((equal? (gi nd) (normalize "reference")) (reference-title-sosofo nd))
       ((equal? (gi nd) (normalize "refsect1")) (refsection-title-sosofo nd))
       ((equal? (gi nd) (normalize "refsect2")) (refsection-title-sosofo nd))
       ((equal? (gi nd) (normalize "refsect3")) (refsection-title-sosofo nd))
       ((equal? (gi nd) (normalize "refsynopsisdiv")) (refsynopsisdiv-title-sosofo nd))
       ((equal? (gi nd) (normalize "sect1")) (section-title-sosofo nd))
       ((equal? (gi nd) (normalize "sect2")) (section-title-sosofo nd))
       ((equal? (gi nd) (normalize "sect3")) (section-title-sosofo nd))
       ((equal? (gi nd) (normalize "sect4")) (section-title-sosofo nd))
       ((equal? (gi nd) (normalize "sect5")) (section-title-sosofo nd))
       ((equal? (gi nd) (normalize "set")) (set-title-sosofo nd))
       (else (block-title-sosofo nd)))))

;; ======================================================================

;; Returns the title of the element; returns a node if possible, or a string
(define (element-title nd)
  (if (node-list-empty? nd)
      ""
      (cond
       ;; Use a seperately defined assoc list?
       ((equal? (gi nd) (normalize "appendix")) (appendix-title nd))
       ((equal? (gi nd) (normalize "article")) (article-title nd))
       ((equal? (gi nd) (normalize "bibliography")) (bibliography-title nd))
       ((equal? (gi nd) (normalize "book")) (book-title nd))
       ((equal? (gi nd) (normalize "chapter")) (chapter-title nd))
       ((equal? (gi nd) (normalize "dedication")) (dedication-title nd))
       ((equal? (gi nd) (normalize "glossary")) (glossary-title nd))
       ((equal? (gi nd) (normalize "index")) (index-title nd))
       ((equal? (gi nd) (normalize "colophon")) (colophon-title nd))
       ((equal? (gi nd) (normalize "setindex")) (setindex-title nd))
       ((equal? (gi nd) (normalize "part")) (part-title nd))
       ((equal? (gi nd) (normalize "preface")) (preface-title nd))
       ((equal? (gi nd) (normalize "refentry")) (refentry-title nd))
       ((equal? (gi nd) (normalize "reference")) (reference-title nd))
       ((equal? (gi nd) (normalize "refsect1")) (refsection-title nd))
       ((equal? (gi nd) (normalize "refsect2")) (refsection-title nd))
       ((equal? (gi nd) (normalize "refsect3")) (refsection-title nd))
       ((equal? (gi nd) (normalize "refsynopsisdiv")) (refsynopsisdiv-title nd))
       ((equal? (gi nd) (normalize "sect1")) (section-title nd))
       ((equal? (gi nd) (normalize "sect2")) (section-title nd))
       ((equal? (gi nd) (normalize "sect3")) (section-title nd))
       ((equal? (gi nd) (normalize "sect4")) (section-title nd))
       ((equal? (gi nd) (normalize "sect5")) (section-title nd))
       ((equal? (gi nd) (normalize "set")) (set-title nd))
       (else (block-title nd)))))

;; ======================================================================
;; Returns the data of a node, carefully excising INDEXTERMs from 
;; the data content
;;

(define (data-of node)
  ;; return the data characters of a node, except for the content of
  ;; indexterms which are suppressed.
  (let loop ((nl (children node)) (result ""))
    (if (node-list-empty? nl)
	result
	(if (equal? (node-property 'class-name (node-list-first nl)) 'element)
	    (if (or (equal? (gi (node-list-first nl)) (normalize "indexterm"))
		    (equal? (gi (node-list-first nl)) (normalize "comment"))
		    (equal? (gi (node-list-first nl)) (normalize "remark")))
		(loop (node-list-rest nl) result)
		(loop (node-list-rest nl)
		      (string-append result (data-of (node-list-first nl)))))
	    (if (or (equal? (node-property 'class-name (node-list-first nl))
			    'data-char)
		    (equal? (node-property 'class-name (node-list-first nl))
			    'sdata))
		(loop (node-list-rest nl)
		      (string-append result (data (node-list-first nl))))
		(loop (node-list-rest nl) result))))))

;; ======================================================================
;; Returns the element title data of nd
;;
(define (element-title-string nd)
  (let ((title (element-title nd)))
    (if (string? title)
	title
	(data-of title))))

;; ======================================================================
;; Returns the element gi as a sosofo
;;
(define (element-gi-sosofo nd)
  (if (node-list-empty? nd)
      (empty-sosofo)
      (make sequence
	(literal (gentext-element-name nd)))))

;; ======================================================================

(define (titlepage-info-elements node info #!optional (intro (empty-node-list)))
  ;; Returns a node-list of the elements that might appear on a title
  ;; page.  This node-list is constructed as follows:
  ;;
  ;; 1. The "title" child of node is considered as a possibility
  ;; 2. If info is not empty, then node-list starts as the children
  ;;    of info.  If the children of info don't include a title, then
  ;;    the title from the node is added.
  ;; 3. If info is empty, then node-list starts as the children of node,
  ;;    but with "partintro" filtered out.

  (let* ((title (select-elements (children node) (normalize "title")))
	 (nl    (if (node-list-empty? info)
		    (node-list-filter-by-not-gi (children node) 
						(list (normalize "partintro")))
		    (children info)))
	 (nltitle (node-list-filter-by-gi nl (list (normalize "title")))))
    (if (node-list-empty? info)
	(node-list nl
		   intro)
	(node-list (if (node-list-empty? nltitle)
		       title
		       (empty-node-list))
		   nl
		   intro))))

;; ======================================================================

(define (info-element #!optional (nd (current-node)))
  ;; Returns the *INFO element for the nd or (empty-node-list) if no
  ;; such node exists...
  (cond
   ((equal? (gi nd) (normalize "set"))
    (select-elements (children nd) (normalize "setinfo")))
   ((equal? (gi nd) (normalize "book"))
    (select-elements (children nd) (normalize "bookinfo")))
   ((equal? (gi nd) (normalize "section"))
    (select-elements (children nd) (normalize "sectioninfo")))
   ((equal? (gi nd) (normalize "sect1"))
    (select-elements (children nd) (normalize "sect1info")))
   ((equal? (gi nd) (normalize "sect2"))
    (select-elements (children nd) (normalize "sect2info")))
   ((equal? (gi nd) (normalize "sect3"))
    (select-elements (children nd) (normalize "sect3info")))
   ((equal? (gi nd) (normalize "sect4"))
    (select-elements (children nd) (normalize "sect4info")))
   ((equal? (gi nd) (normalize "sect5"))
    (select-elements (children nd) (normalize "sect5info")))
   ((equal? (gi nd) (normalize "refsect1"))
    (select-elements (children nd) (normalize "refsect1info")))
   ((equal? (gi nd) (normalize "refsect2"))
    (select-elements (children nd) (normalize "refsect2info")))
   ((equal? (gi nd) (normalize "refsect3"))
    (select-elements (children nd) (normalize "refsect3info")))
   ((equal? (gi nd) (normalize "refsynopsisdiv"))
    (select-elements (children nd) (normalize "refsynopsisdivinfo")))
   ((equal? (gi nd) (normalize "article"))
    (node-list-filter-by-gi (children nd) (list
					   (normalize "artheader")
					   (normalize "articleinfo"))))
   (else ;; BIBLIODIV, GLOSSDIV, INDEXDIV, PARTINTRO, SIMPLESECT
    (select-elements (children nd) (normalize "docinfo")))))

;; ======================================================================
;;
;; Bibliography filtering...

(define (biblio-filter allentries)
  (let* ((all  (descendants (sgml-root-element)))
	 (link (select-elements all (normalize "link")))
	 (xref (select-elements all (normalize "xref")))
	 (cite (select-elements all (normalize "citation")))
	 (xref-elements (node-list link xref)))
    (let loop ((entries allentries) (used (empty-node-list)))
      (if (node-list-empty? entries)
	  used
	  (if (or (cited-by-xref (node-list-first entries) xref-elements)
		  (cited-by-citation (node-list-first entries) cite))
	      (loop (node-list-rest entries) 
		    (node-list used (node-list-first entries)))
	      (loop (node-list-rest entries) used))))))

(define (cited-by-xref bib xref-elements)
  (let* ((id (attribute-string (normalize "id") bib)))
    (if id
	(let loop ((links xref-elements))
	  (if (node-list-empty? links)
	      #f
	      (if (equal? (attribute-string (normalize "linkend") 
					    (node-list-first links)) id)
		  #t
		  (loop (node-list-rest links)))))
	#f)))

(define (cited-by-citation bib citations)
  (let loop ((links citations))
    (if (node-list-empty? links)
	#f
	(if (citation-matches-target? (node-list-first links) bib)
	    #t
	    (loop (node-list-rest links))))))

(define (citation-matches-target? citation target)
  (let* ((fchild (node-list-first 
		  (node-list-filter-out-pis 
		   (children target))))
	 (abbrev (if (equal? (gi fchild) (normalize "abbrev"))
		     fchild
		     (empty-node-list)))
	 (cite   (data-of citation)))
    (or (equal? (attribute-string "id" target) (normalize cite))
	(equal? (attribute-string "xreflabel" target) (normalize cite))
	(equal? (normalize cite) (normalize (data-of abbrev))))))

(define (bibentry-number bibentry)
  (let* ((bgraphy (ancestor-member bibentry 
				   (list (normalize "bibliography"))))
	 (comps   (expand-children (children bgraphy) 
				   (list (normalize "bibliodiv")))))
    (let loop ((nl comps) (count 1))
      (if (node-list-empty? nl) 
	  0
	  (if (node-list=? (node-list-first nl) bibentry)
	      count
	      (if (or (equal? (gi (node-list-first nl))
			      (normalize "biblioentry"))
		      (equal? (gi (node-list-first nl))
			      (normalize "bibliomixed")))
		  (loop (node-list-rest nl) (+ count 1))
		  (loop (node-list-rest nl) count)))))))

;; ======================================================================

(define (olink-resource-title pubid sysid)
  ;; This version of olink-resource-title expects public identifiers
  ;; with the following format:
  ;;
  ;;   -//owner//TEXT title Vx.x//EN
  ;; 
  ;; Specifically the title is the description field of the public
  ;; identifier minus the first word (TEXT, the type) and the last
  ;; word, in my case a version string.  Words are blank delimited.
  ;; The parsing will fail if a "/" appears anywhere in any field.
  ;; The system identifier is ignored
  ;; 
  (let* ((pubidparts   (if pubid
			   (split pubid '(#\/))
			   (split "-//none//type version//la" '(#\/))))
	 (description  (car (cdr (cdr pubidparts))))
	 (descparts    (split description))
	 (titleparts   (list-head (cdr descparts) (- (length descparts) 2))))
    (join titleparts)))

;; ======================================================================

(define (orderedlist-listitem-number listitem)
  ;; return the number of listitem, taking continuation into account
  (let* ((orderedlist (parent listitem))
	 (listitems (select-elements (children orderedlist)
				     (normalize "listitem")))
	 (continue? (equal? (attribute-string (normalize "continuation")
					      orderedlist)
			    (normalize "continues")))

;; If a list is the continuation of a previous list, we must find the
;; list that is continued in order to calculate the starting
;; item number of this list.
;;
;; Of all the lists in this component, only the following are candidates:
;; 1. Lists which precede this list
;; 2. Lists which are not ancestors of this list
;; 3. Lists that do not have ancestors that are lists which precede this one
;;
;; Of the candidates, the last one, in document order, is the preceding
;; list
	 (all-lists (select-elements
		     (descendants (ancestor-member orderedlist
						   (component-element-list)))
		     (normalize "orderedlist")))

	 (cand1     (if continue?
			(let loop ((nl all-lists)
				   (prec (empty-node-list)))
			  (if (node-list-empty? nl)
			      prec
			      (if (node-list=? (node-list-first nl)
					       orderedlist)
				  prec
				  (loop (node-list-rest nl)
					(node-list prec
						   (node-list-first nl))))))
			(empty-node-list)))

	 (cand2     (let loop ((nl cand1)
			       (cand2lists (empty-node-list)))
		      (if (node-list-empty? nl)
			  cand2lists
			  (loop (node-list-rest nl)
				(if (descendant-of? (node-list-first nl)
						    orderedlist)
				    cand2lists
				    (node-list cand2lists
					       (node-list-first nl)))))))

	 ;; now find the last item of cand2 that is not a descendant
	 ;; of some other element of the cand2 list.
	 (preclist  (let loop ((nl (node-list-reverse cand2)))
		      (if (node-list-empty? nl)
			  (empty-node-list)
			  (if (descendant-member-of?
			       (node-list-first nl)
			       (node-list-rest nl))
			      (loop (node-list-rest nl))
			      (node-list-first nl)))))

	 (precitem (if (node-list-empty? preclist)
		       (empty-node-list)
		       (node-list-last (children preclist))))
	 (precitem-number (if (and continue? (not (node-list-empty? precitem)))
			      (orderedlist-listitem-number precitem)
			      0)))

    (+ precitem-number (child-number listitem))))

(define (descendant-member-of? node node-list)
  ;; return true if node is a descedant of any member of node-list
  (let loop ((nl node-list))
    (if (node-list-empty? nl)
	#f
	(if (descendant-of? (node-list-first nl) node)
	    #t
	    (loop (node-list-rest nl))))))

;; ======================================================================

(define (orderedlist-listitem-label listitem)
  ;; return the formatted number of listitem
  (let* ((number  (orderedlist-listitem-number listitem))
	 (depth   (length (hierarchical-number-recursive
			   (normalize "orderedlist")
			   listitem)))
	 (numeration (inherited-attribute-string (normalize "numeration")
						 listitem))
	 ;; rawnum allows for numbering to alternate
	 (rawnum (cond
		  ((equal? numeration (normalize "arabic")) 1)
		  ((equal? numeration (normalize "loweralpha")) 2)
		  ((equal? numeration (normalize "lowerroman")) 3)
		  ((equal? numeration (normalize "upperalpha")) 4)
		  ((equal? numeration (normalize "upperroman")) 0)
		  (else (modulo depth 5)))))
    (case rawnum
      ((1) (format-number number "1"))
      ((2) (format-number number "a"))
      ((3) (format-number number "i"))
      ((4) (format-number number "A"))
      ((0) (format-number number "I")))))

(define (orderedlist-listitem-label-recursive listitem)
  ;; return the recursively formatted number of the listitem.
  ;; In other words, something of the form 1.2.3 for a third level
  ;; nested ordered list
  (let loop ((li (parent listitem)) 
	     (label (orderedlist-listitem-label listitem)))
    (if (or (node-list-empty? li)
	    (node-list-empty? (ancestor (normalize "orderedlist") li)))
	label
	(if (and (equal? (gi li) (normalize "listitem"))
		 (equal? (gi (parent li)) (normalize "orderedlist")))
	    (loop (parent li)
		  (string-append 
		   (orderedlist-listitem-label li)
		   (gentext-intra-label-sep li)
		   label))
	    (loop (parent li) label)))))

(define (question-answer-label #!optional (node (current-node)))
  (let* ((inhlabel (inherited-attribute-string (normalize "defaultlabel")
					       node))
	 (deflabel (if inhlabel inhlabel (qanda-defaultlabel)))
	 (label    (attribute-string (normalize "label") node))
	 (hnr      (hierarchical-number-recursive (normalize "qandadiv")
						  node))

	 (parsect  (ancestor-member node (section-element-list)))

	 (defnum   (if (and %qanda-inherit-numeration% 
			    %section-autolabel%)
		       (if (node-list-empty? parsect)
			   (section-autolabel-prefix node)
			   (section-autolabel parsect))
		       ""))

	 (hnumber  (let loop ((numlist hnr) (number defnum) 
			      (sep (if (equal? defnum "") "" ".")))
		     (if (null? numlist)
			 number
			 (loop (cdr numlist) 
			       (string-append number
					      sep
					      (number->string (car numlist)))
			       "."))))
	 (cnumber  (child-number (parent node)))
	 (number   (string-append hnumber 
				  (if (equal? hnumber "")
				      ""
				      ".")
				  (number->string cnumber))))
    (cond
     ((equal? deflabel (normalize "qanda"))
      (gentext-element-name node))
     ((equal? deflabel (normalize "label"))
      label)
     ;; Note: only questions are numbered...
     ((and (equal? deflabel (normalize "number"))
	   (equal? (gi node) (normalize "question")))
      (string-append number "."))
     (else ""))))

;; ======================================================================
;; Calculate term lengths...

(define (varlistentry-term-too-long? vle termlength)
  (let loop ((nl (select-elements (children vle) (normalize "term")))
	     (too-long? #f))
    (if (or too-long? (node-list-empty? nl))
	too-long?
	(loop (node-list-rest nl)
	      (> (string-length (data (node-list-first nl)))
		 termlength)))))

(define (variablelist-term-too-long? termlength)
  (let loop ((nl (select-elements (children (current-node))
				  (normalize "varlistentry")))
	     (too-long? #f))
    (if (or too-long? (node-list-empty? nl))
	too-long?
	(loop (node-list-rest nl)
	      (varlistentry-term-too-long? (node-list-first nl) termlength)))))

;; ======================================================================
;; bibliography elements

(define (biblioentry-inline-elements)
  (list (normalize "abbrev")
	(normalize "affiliation")
	(normalize "artpagenums")
	(normalize "author")
	(normalize "authorgroup")
	(normalize "authorinitials")
	(normalize "citetitle")
	(normalize "collab")
	(normalize "confgroup")
	(normalize "contractnum")
	(normalize "contractsponsor")
	(normalize "contrib")
	(normalize "copyright")
	(normalize "corpauthor")
	(normalize "corpname")
	(normalize "date")
	(normalize "edition")
	(normalize "editor")
	(normalize "firstname")
	(normalize "honorific")
	(normalize "invpartnumber")
	(normalize "isbn")
	(normalize "issn")
	(normalize "issuenum")
	(normalize "lineage")
	(normalize "orgname")
	(normalize "othercredit")
	(normalize "othername")
	(normalize "pagenums")
	(normalize "productname")
	(normalize "productnumber")
	(normalize "pubdate")
	(normalize "publisher")
	(normalize "publishername")
	(normalize "pubsnumber")
	(normalize "releaseinfo")
	(normalize "seriesvolnums")
	(normalize "subtitle")
	(normalize "surname")
	(normalize "title")
	(normalize "titleabbrev")
	(normalize "volumenum")))

(define (biblioentry-block-elements)
  (list (normalize "abstract")
	(normalize "address")
	(normalize "authorblurb")
	(normalize "printhistory")
	(normalize "revhistory")
	(normalize "seriesinfo")))

(define (biblioentry-flatten-elements)
  (list (normalize "artheader")
	(normalize "biblioset")
	(normalize "bookbiblio")))

;; === db31 common ======================================================

(define (data-filename dataobj)
  (let* ((entityref (attribute-string (normalize "entityref") dataobj))
	 (fileref   (attribute-string (normalize "fileref") dataobj))
	 (filename  (if fileref
			fileref
			(system-id-filename entityref)))
	 (ext       (file-extension filename)))
    (if (or (not filename)
	    (not %graphic-default-extension%)
	    (member ext %graphic-extensions%))
	filename
	(string-append filename "." %graphic-default-extension%))))

(define (normalized-member string string-list)
  (if (string? string)
      (let loop ((sl string-list))
	(if (null? sl)
	    #f
	    (if (string=? (normalize string) (normalize (car sl)))
		#t
		(loop (cdr sl)))))
      #f))

(define (find-displayable-object objlist notlist extlist)
  (let loop ((nl objlist))
    (if (node-list-empty? nl)
	(empty-node-list)
	(let* ((objdata  (node-list-filter-by-gi
			  (children (node-list-first nl))
			  (list (normalize "videodata")
				(normalize "audiodata")
				(normalize "imagedata"))))
	       (filename  (data-filename objdata))
	       (extension (file-extension filename))
	       (notation  (attribute-string (normalize "format") objdata)))
	  (if (or (normalized-member notation notlist)
		  (normalized-member extension extlist)
		  (and notation
		       (string=? notation (normalize "linespecific"))))
	      (node-list-first nl)
	      (loop (node-list-rest nl)))))))

(define (select-displayable-object objlist)
  (let ((pref (find-displayable-object objlist
				       preferred-mediaobject-notations
				       preferred-mediaobject-extensions))
	(ok   (find-displayable-object objlist
				       acceptable-mediaobject-notations
				       acceptable-mediaobject-extensions)))
    (if (node-list-empty? pref)
	ok
	pref)))

(define ($mediaobject$)
  (let* ((objects (node-list-filter-by-gi
		   (children (current-node))
		   (list (normalize "videoobject")
			 (normalize "imageobject")
			 (normalize "audioobject"))))
	 (dobject (select-displayable-object objects))
	 (textobj (select-elements (children (current-node))
				   (normalize "textobject")))
	 (caption (select-elements (children (current-node))
				   (normalize "caption"))))
    (make sequence
      (if (node-list-empty? dobject)
	  (if (node-list-empty? textobj)
	      (empty-sosofo)
	      (process-node-list (node-list-first textobj)))
	  (process-node-list dobject))
      (process-node-list caption))))

;; ======================================================================
