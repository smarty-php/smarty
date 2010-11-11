<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % el.words
  PUBLIC "-//Norman Walsh//ENTITIES DocBook Stylesheet Localization//EL"
         "dbl1el.ent">
%el.words;
]>

<style-sheet>
<style-specification id="docbook-l10n-el">
<style-specification-body>

;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; ----------------------------- Localization -----------------------------

;; If you create a new version of this file, please send it to
;; Norman Walsh, ndw@nwalsh.com

;; The generated text for cross references to elements.  See dblink.dsl
;; for a discussion of how substitution is performed on the %x 
;; keywords.
;;

(define (el-author-string #!optional (author (current-node)))
  ;; Return a formatted string representation of the contents of:
  ;; AUTHOR:
  ;;   Handles Honorific, FirstName, SurName, and Lineage.
  ;;     If %author-othername-in-middle% is #t, also OtherName
  ;;   Handles *only* the first of each.
  ;;   Format is "Honorific. FirstName [OtherName] SurName, Lineage"
  ;; CORPAUTHOR:
  ;;   returns (data corpauthor)
  (let* ((h_nl (select-elements (descendants author) (normalize "honorific")))
	 (f_nl (select-elements (descendants author) (normalize "firstname")))
	 (o_nl (select-elements (descendants author) (normalize "othername")))
	 (s_nl (select-elements (descendants author) (normalize "surname")))
	 (l_nl (select-elements (descendants author) (normalize "lineage")))
	 (has_h (not (node-list-empty? h_nl)))
	 (has_f (not (node-list-empty? f_nl)))
	 (has_o (and %author-othername-in-middle%
		     (not (node-list-empty? o_nl))))
	 (has_s (not (node-list-empty? s_nl)))
	 (has_l (not (node-list-empty? l_nl))))
    (if (or (equal? (gi author) (normalize "author"))
	    (equal? (gi author) (normalize "editor"))
	    (equal? (gi author) (normalize "othercredit")))
	(string-append
	 (if has_h (string-append (data-of (node-list-first h_nl)) 
				  %honorific-punctuation%) "")
	 (if has_f (string-append 
		    (if has_h " " "") 
		    (data-of (node-list-first f_nl))) "")
	 (if has_o (string-append
		    (if (or has_h has_f) " " "")
		    (data-of (node-list-first o_nl))) "")
	 (if has_s (string-append 
		    (if (or has_h has_f has_o) " " "")
		    (data-of (node-list-first s_nl))) "")
	 (if has_l (string-append ", " (data-of (node-list-first l_nl))) ""))
	(data-of author))))

(define (el-xref-strings)
  (list (list (normalize "appendix")    (if %chapter-autolabel%
					    "&Appendix; %n"
					    "&#244;&#239; &appendix; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "article")     (string-append %gentext-el-start-quote%
						       "%t"
						       %gentext-el-end-quote%))
	(list (normalize "bibliography") "%t")
	(list (normalize "book")        "%t")
	(list (normalize "chapter")     (if %chapter-autolabel%
					    "&Chapter; %n"
					    "&#244;&#239; &chapter; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "equation")    "&Equation; %n")
	(list (normalize "example")     "&Example; %n")
	(list (normalize "figure")      "&Figure; %n")
	(list (normalize "glossary")    "%t")
	(list (normalize "index")       "%t")
	(list (normalize "listitem")    "%n")
	(list (normalize "part")        "&Part; %n")
	(list (normalize "preface")     "%t")
	(list (normalize "procedure")   "&Procedure; %n, %t")
	(list (normalize "reference")   "&Reference; %n, %t")
	(list (normalize "section")     (if %section-autolabel%
					    "&Section; %n"
					    "&#244;&#239; &section; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "sect1")       (if %section-autolabel%
					    "&Section; %n"
					    "&#244;&#239; &section; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "sect2")       (if %section-autolabel%
					    "&Section; %n"
					    "&#244;&#239; &section; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "sect3")       (if %section-autolabel%
					    "&Section; %n"
					    "&#244;&#239; &section; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "sect4")       (if %section-autolabel%
					    "&Section; %n"
					    "&#244;&#239; &section; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "sect5")       (if %section-autolabel%
					    "&Section; %n"
					    "&#244;&#239; &section; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "simplesect")  (if %section-autolabel%
					    "&Section; %n"
					    "&#244;&#239; &section; &#236;&#229; &#252;&#237;&#239;&#236;&#225; %t"))
	(list (normalize "sidebar")     "&#244;&#239; &sidebar; %t")
	(list (normalize "step")        "&step; %n")
	(list (normalize "table")       "&Table; %n")))


(define (gentext-el-xref-strings gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname))
	 (xref   (assoc name (el-xref-strings))))
    (if xref
	(car (cdr xref))
	(let* ((msg    (string-append "[&xrefto; "
				      (if giname giname "&nonexistantelement;")
				      " &unsupported;]"))
	       (err    (node-list-error msg (current-node))))
	  msg))))

(define (el-auto-xref-indirect-connector before) 
  ;; In English, the (cond) is unnecessary since the word is always the
  ;; same, but in other languages, that's not the case.  I've set this
  ;; one up with the (cond) so it stands as an example.
  (cond 
   ((equal? (gi before) (normalize "book"))
    (literal " &in; "))
   ((equal? (gi before) (normalize "chapter"))
    (literal " &in; "))
   ((equal? (gi before) (normalize "sect1"))
    (literal " &in; "))
   (else
    (literal " &in; "))))

;; Should the TOC come first or last?
;;
(define %generate-el-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define (el-element-name)
  (list
   (list (normalize "abstract")		"&Abstract;")
   (list (normalize "answer")		"&Answer;")
   (list (normalize "appendix")		"&Appendix;")
   (list (normalize "article")		"&Article;")
   (list (normalize "bibliography")	"&Bibliography;")
   (list (normalize "book")		"&Book;")
   (list (normalize "calloutlist")	"")
   (list (normalize "caution")		"&Caution;")
   (list (normalize "chapter")		"&Chapter;")
   (list (normalize "copyright")	"&Copyright;")
   (list (normalize "dedication")	"&Dedication;")
   (list (normalize "edition")		"&Edition;")
   (list (normalize "equation")		"&Equation;")
   (list (normalize "example")		"&Example;")
   (list (normalize "figure")		"&Figure;")
   (list (normalize "glossary")		"&Glossary;")
   (list (normalize "glosssee")		"&GlossSee;")
   (list (normalize "glossseealso")	"&GlossSeeAlso;")
   (list (normalize "important")	"&Important;")
   (list (normalize "index")		"&Index;")
   (list (normalize "colophon")		"&Colophon;")
   (list (normalize "setindex")		"&SetIndex;")
   (list (normalize "isbn")		"&isbn;")
   (list (normalize "legalnotice")	"&LegalNotice;")
   (list (normalize "msgaud")		"&MsgAud;")
   (list (normalize "msglevel")		"&MsgLevel;")
   (list (normalize "msgorig")		"&MsgOrig;")
   (list (normalize "note")		"&Note;")
   (list (normalize "part")		"&Part;")
   (list (normalize "preface")		"&Preface;")
   (list (normalize "procedure")	"&Procedure;")
   (list (normalize "pubdate")		"&Published;")
   (list (normalize "question")		"&Question;")
   (list (normalize "refentry")		"&RefEntry;")
   (list (normalize "reference")	"&Reference;")
   (list (normalize "refname")		"&RefName;")
   (list (normalize "revhistory")	"&RevHistory;")
   (list (normalize "refsect1")		"&RefSection;")
   (list (normalize "refsect2")		"&RefSection;")
   (list (normalize "refsect3")		"&RefSection;")
   (list (normalize "refsynopsisdiv")	"&RefSynopsisDiv;")
   (list (normalize "revision")		"&Revision;")
   (list (normalize "sect1")		"&Section;")
   (list (normalize "sect2")		"&Section;")
   (list (normalize "sect3")		"&Section;")
   (list (normalize "sect4")		"&Section;")
   (list (normalize "sect5")		"&Section;")
   (list (normalize "section")		"&Section;")
   (list (normalize "simplesect")	"&Section;")
   (list (normalize "seeie")		"&See;")
   (list (normalize "seealsoie")	"&Seealso;")
   (list (normalize "set")		"&Set;")
   (list (normalize "sidebar")		"&Sidebar;")
   (list (normalize "step")		"&step;")
   (list (normalize "table")		"&Table;")
   (list (normalize "tip")		"&Tip;")
   (list (normalize "toc")		"&TableofContents;")
   (list (normalize "warning")		"&Warning;")
   ))

(define (gentext-el-element-name gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname))
	 (pname  (assoc name (el-element-name))))
    (if pname
	(car (cdr pname))
	(let* ((msg (string-append 
		     "gentext-el-element-name: &unexpectedelementname;: "
		     name))
	       (err (node-list-error msg (current-node))))
	  msg))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-el-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;

(define (local-el-intra-label-sep)
  (list))

(define (el-intra-label-sep)
  (list
   (list (normalize "equation")		"-")
   (list (normalize "informalequation")	"-")
   (list (normalize "example")		"-")
   (list (normalize "figure")		"-")
   (list (normalize "listitem")		".")
   (list (normalize "procedure")	".")
   (list (normalize "refentry")		".")
   (list (normalize "reference")	".")
   (list (normalize "refname")		", ")
   (list (normalize "refsect1")		".")
   (list (normalize "refsect2")		".")
   (list (normalize "refsect3")		".")
   (list (normalize "sect1")		".")
   (list (normalize "sect2")		".")
   (list (normalize "sect3")		".")
   (list (normalize "sect4")		".")
   (list (normalize "sect5")		".")
   (list (normalize "section")		".")
   (list (normalize "simplesect")	".")
   (list (normalize "step")		".")
   (list (normalize "table")		"-")
   (list (normalize "_pagenumber")	"-")
   ))

(define (gentext-el-intra-label-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname))
	 (lsep   (assoc name (local-el-intra-label-sep)))
	 (sep    (assoc name (el-intra-label-sep))))
    (if lsep
        (car (cdr lsep))
        (if sep
            (car (cdr sep))
            ""))))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;

(define (local-el-label-title-sep)
  (list))

(define (el-label-title-sep)
  (list
   (list (normalize "abstract")		": ")
   (list (normalize "answer")		" ")
   (list (normalize "appendix")		". ")
   (list (normalize "caution")		"")
   (list (normalize "chapter")		". ")
   (list (normalize "equation")		". ")
   (list (normalize "example")		". ")
   (list (normalize "figure")		". ")
   (list (normalize "footnote")		". ")
   (list (normalize "glosssee")		": ")
   (list (normalize "glossseealso")	": ")
   (list (normalize "important")	": ")
   (list (normalize "note")		": ")
   (list (normalize "orderedlist")	". ")
   (list (normalize "part")		". ")
   (list (normalize "procedure")	". ")
   (list (normalize "prefix")		". ")
   (list (normalize "question")		" ")
   (list (normalize "refentry")		"")
   (list (normalize "reference")	". ")
   (list (normalize "refsect1")		". ")
   (list (normalize "refsect2")		". ")
   (list (normalize "refsect3")		". ")
   (list (normalize "sect1")		". ")
   (list (normalize "sect2")		". ")
   (list (normalize "sect3")		". ")
   (list (normalize "sect4")		". ")
   (list (normalize "sect5")		". ")
   (list (normalize "section")		". ")
   (list (normalize "simplesect")	". ")
   (list (normalize "seeie")		" ")
   (list (normalize "seealsoie")	" ")
   (list (normalize "step")		". ")
   (list (normalize "table")		". ")
   (list (normalize "tip")		": ")
   (list (normalize "warning")		"")
   ))

(define (gentext-el-label-title-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname))
	 (lsep   (assoc name (local-el-label-title-sep)))
	 (sep    (assoc name (el-label-title-sep))))
    (if lsep
      (car (cdr lsep))
      (if sep
  	  (car (cdr sep))
   	  ""))))

(define (el-label-number-format-list)
  (list
   (list (normalize "set")		"1")
   (list (normalize "book")		"1")
   (list (normalize "prefix")		"1")
   (list (normalize "part")		"I")
   (list (normalize "chapter")		"1")
   (list (normalize "appendix")		"A")
   (list (normalize "reference")	"I")
   (list (normalize "example")		"1")
   (list (normalize "figure")		"1")
   (list (normalize "table")		"1")
   (list (normalize "procedure")	"1")
   (list (normalize "step")		"1")
   (list (normalize "refsect1")		"1")
   (list (normalize "refsect2")		"1")
   (list (normalize "refsect3")		"1")
   (list (normalize "sect1")		"1")
   (list (normalize "sect2")		"1")
   (list (normalize "sect3")		"1")
   (list (normalize "sect4")		"1")
   (list (normalize "sect5")		"1")
   (list (normalize "section")		"1")
   ))

(define (el-label-number-format gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname))
	 (format (assoc name (el-label-number-format-list))))
    (if format
	(car (cdr format))
	"1")))

(define (el-lot-title)
  (list
   (list (normalize "table")		"&ListofTables;")
   (list (normalize "example")		"&ListofExamples;")
   (list (normalize "figure")		"&ListofFigures;")
   (list (normalize "equation")		"&ListofEquations;")
   ))

(define ($lot-title-el$ gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname))
	 (title  (assoc name (el-lot-title))))
    (if title
	(car (cdr title))
	(let* ((msg (string-append "&ListofUnknown;: " name))
	       (err (node-list-error msg (current-node))))
	  msg))))

(define %gentext-el-start-quote% (dingbat "ldquo"))

(define %gentext-el-end-quote% (dingbat "rdquo"))

(define %gentext-el-start-nested-quote% (dingbat "lsquo"))

(define %gentext-el-end-nested-quote% (dingbat "rsquo"))

(define %gentext-el-by% "&by;") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-el-edited-by% "&Editedby;")
                           ;; "Edited by" Jane Doe

(define %gentext-el-revised-by% "&Editedby;")
                           ;; "Revised by" Jane Doe

(define %gentext-el-page% "")

(define %gentext-el-and% "&and;")

(define %gentext-el-listcomma% "&listcomma;")

(define %gentext-el-lastlistcomma% "&lastlistcomma;")

(define %gentext-el-bibl-pages% "&Pgs;")

(define %gentext-el-endnotes% "&Notes;")

(define %gentext-el-table-endnotes% "&TableNotes;:")

(define %gentext-el-index-see% "&See;")

(define %gentext-el-index-seealso% "&SeeAlso;")


(define (gentext-el-nav-prev prev) 
  (make sequence (literal "&nav-prev;")))

(define (gentext-el-nav-prev-sibling prevsib) 
  (make sequence (literal "&nav-prev-sibling;")))

(define (gentext-el-nav-next-sibling nextsib)
  (make sequence (literal "&nav-next-sibling;")))

(define (gentext-el-nav-next next)
  (make sequence (literal "&nav-next;")))

(define (gentext-el-nav-up up)
  (make sequence (literal "&nav-up;")))

(define (gentext-el-nav-home home)
  (make sequence (literal "&nav-home;")))



</style-specification-body>
</style-specification>
</style-sheet>
