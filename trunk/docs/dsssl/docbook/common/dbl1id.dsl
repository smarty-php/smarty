<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % id.words
  PUBLIC "-//Norman Walsh//ENTITIES DocBook Stylesheet Localization//IN"
         "dbl1id.ent">
%id.words;
]>

<style-sheet>
<style-specification id="docbook-l10n-id">
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
;; Contributors:
;; Mohammad DAMT, mdamt@cdl2000.com
;;

(define (id-author-string #!optional (author (current-node)))
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

(define (id-appendix-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Appendix; %n"
      "&appendix; berjudul %t"))

(define (id-article-xref-string gi-or-name)
  (string-append %gentext-id-start-quote%
		 "%t"
		 %gentext-id-end-quote%))

(define (id-bibliography-xref-string gi-or-name)
  "%t")

(define (id-book-xref-string gi-or-name)
  "%t")

(define (id-chapter-xref-string gi-or-name)
  (if %chapter-autolabel%
      "&Chapter; %n"
      "&chapter; berjudul %t"))

(define (id-equation-xref-string gi-or-name)
  "&Equation; %n")

(define (id-example-xref-string gi-or-name)
  "&Example; %n")

(define (id-figure-xref-string gi-or-name)
  "&Figure; %n")

(define (id-glossary-xref-string gi-or-name)
  "%t")

(define (id-index-xref-string gi-or-name)
  "%t")

(define (id-listitem-xref-string gi-or-name)
  "%n")

(define (id-part-xref-string gi-or-name)
  "&Part; %n")

(define (id-preface-xref-string gi-or-name)
  "%t")

(define (id-procedure-xref-string gi-or-name)
  "&Procedure; %n, %t")

(define (id-reference-xref-string gi-or-name)
  "&Reference; %n, %t")

(define (id-sectioning-xref-string gi-or-name)
  (if %section-autolabel% 
      "&Section; %n" 
      "&section; berjudul %t"))

(define (id-sect1-xref-string gi-or-name)
  (id-sectioning-xref-string gi-or-name))

(define (id-sect2-xref-string gi-or-name)
  (id-sectioning-xref-string gi-or-name))

(define (id-sect3-xref-string gi-or-name)
  (id-sectioning-xref-string gi-or-name))

(define (id-sect4-xref-string gi-or-name)
  (id-sectioning-xref-string gi-or-name))

(define (id-sect5-xref-string gi-or-name)
  (id-sectioning-xref-string gi-or-name))

(define (id-section-xref-string gi-or-name)
  (id-sectioning-xref-string gi-or-name))

(define (id-sidebar-xref-string gi-or-name)
  "the &sidebar; %t")

(define (id-step-xref-string gi-or-name)
  "&step; %n")

(define (id-table-xref-string gi-or-name)
  "&Table; %n")

(define (id-default-xref-string gi-or-name)
  (let* ((giname (if (string? gi-or-name) gi-or-name (gi gi-or-name)))
	 (msg    (string-append "[&xrefto; "
				(if giname giname "&nonexistantelement;")
				" &unsupported;]"))
	 (err    (node-list-error msg (current-node))))
    msg))

(define (gentext-id-xref-strings gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
      ((equal? name (normalize "appendix")) (id-appendix-xref-string gind))
      ((equal? name (normalize "article"))  (id-article-xref-string gind))
      ((equal? name (normalize "bibliography")) (id-bibliography-xref-string gind))
      ((equal? name (normalize "book"))     (id-book-xref-string gind))
      ((equal? name (normalize "chapter"))  (id-chapter-xref-string gind))
      ((equal? name (normalize "equation")) (id-equation-xref-string gind))
      ((equal? name (normalize "example"))  (id-example-xref-string gind))
      ((equal? name (normalize "figure"))   (id-figure-xref-string gind))
      ((equal? name (normalize "glossary")) (id-glossary-xref-string gind))
      ((equal? name (normalize "index"))    (id-index-xref-string gind))
      ((equal? name (normalize "listitem")) (id-listitem-xref-string gind))
      ((equal? name (normalize "part"))     (id-part-xref-string gind))
      ((equal? name (normalize "preface"))  (id-preface-xref-string gind))
      ((equal? name (normalize "procedure")) (id-procedure-xref-string gind))
      ((equal? name (normalize "reference")) (id-reference-xref-string gind))
      ((equal? name (normalize "sect1"))    (id-sect1-xref-string gind))
      ((equal? name (normalize "sect2"))    (id-sect2-xref-string gind))
      ((equal? name (normalize "sect3"))    (id-sect3-xref-string gind))
      ((equal? name (normalize "sect4"))    (id-sect4-xref-string gind))
      ((equal? name (normalize "sect5"))    (id-sect5-xref-string gind))
      ((equal? name (normalize "section"))  (id-section-xref-string gind))
      ((equal? name (normalize "simplesect"))  (id-section-xref-string gind))
      ((equal? name (normalize "sidebar"))  (id-sidebar-xref-string gind))
      ((equal? name (normalize "step"))     (id-step-xref-string gind))
      ((equal? name (normalize "table"))    (id-table-xref-string gind))
      (else (id-default-xref-string gind)))))

(define (id-auto-xref-indirect-connector before) 
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
(define %generate-id-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define id-abstract-name	"&Abstract;")
(define id-answer-name		"&Answer;")
(define id-appendix-name	"&Appendix;")
(define id-article-name	"&Article;")
(define id-bibliography-name	"&Bibliography;")
(define id-book-name		"&Book;")
(define id-calloutlist-name	"")
(define id-caution-name	"&Caution;")
(define id-chapter-name	"&Chapter;")
(define id-copyright-name	"&Copyright;")
(define id-dedication-name	"&Dedication;")
(define id-edition-name	"&Edition;")
(define id-equation-name	"&Equation;")
(define id-example-name	"&Example;")
(define id-figure-name	"&Figure;")
(define id-glossary-name	"&Glossary;")
(define id-glosssee-name	"&GlossSee;")
(define id-glossseealso-name	"&GlossSeeAlso;")
(define id-important-name	"&Important;")
(define id-index-name		"&Index;")
(define id-colophon-name	"&Colophon;")
(define id-setindex-name	"&SetIndex;")
(define id-isbn-name		"&isbn;")
(define id-legalnotice-name	"&LegalNotice;")
(define id-msgaud-name	"&MsgAud;")
(define id-msglevel-name	"&MsgLevel;")
(define id-msgorig-name	"&MsgOrig;")
(define id-note-name		"&Note;")
(define id-part-name		"&Part;")
(define id-preface-name	"&Preface;")
(define id-procedure-name	"&Procedure;")
(define id-pubdate-name	"&Published;")
(define id-question-name	"&Question;")
(define id-refentry-name      "&RefEntry;")
(define id-reference-name	"&Reference;")
(define id-refname-name	"&RefName;")
(define id-revhistory-name	"&RevHistory;")
(define id-refsect1-name      "&RefSection;")
(define id-refsect2-name      "&RefSection;")
(define id-refsect3-name      "&RefSection;")
(define id-refsynopsisdiv-name      "&RefSynopsisDiv;")
(define id-revision-name	"&Revision;")
(define id-sect1-name		"&Section;")
(define id-sect2-name		"&Section;")
(define id-sect3-name		"&Section;")
(define id-sect4-name		"&Section;")
(define id-sect5-name		"&Section;")
(define id-section-name		"&Section;")
(define id-simplesect-name	"&Section;")
(define id-seeie-name		"&See;")
(define id-seealsoie-name	"&Seealso;")
(define id-set-name		"&Set;")
(define id-sidebar-name	"&Sidebar;")
(define id-step-name		"&step;")
(define id-table-name		"&Table;")
(define id-tip-name		"&Tip;")
(define id-toc-name		"&TableofContents;")
(define id-warning-name	"&Warning;")

(define (gentext-id-element-name gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract"))	id-abstract-name)
     ((equal? name (normalize "answer"))	id-answer-name)
     ((equal? name (normalize "appendix"))	id-appendix-name)
     ((equal? name (normalize "article"))	id-article-name)
     ((equal? name (normalize "bibliography"))	id-bibliography-name)
     ((equal? name (normalize "book"))		id-book-name)
     ((equal? name (normalize "calloutlist"))	id-calloutlist-name)
     ((equal? name (normalize "caution"))	id-caution-name)
     ((equal? name (normalize "chapter"))	id-chapter-name)
     ((equal? name (normalize "copyright"))	id-copyright-name)
     ((equal? name (normalize "dedication"))	id-dedication-name)
     ((equal? name (normalize "edition"))	id-edition-name)
     ((equal? name (normalize "equation"))	id-equation-name)
     ((equal? name (normalize "example"))	id-example-name)
     ((equal? name (normalize "figure"))	id-figure-name)
     ((equal? name (normalize "glossary"))	id-glossary-name)
     ((equal? name (normalize "glosssee"))	id-glosssee-name)
     ((equal? name (normalize "glossseealso"))	id-glossseealso-name)
     ((equal? name (normalize "important"))	id-important-name)
     ((equal? name (normalize "index"))		id-index-name)
     ((equal? name (normalize "colophon"))	id-colophon-name)
     ((equal? name (normalize "setindex"))	id-setindex-name)
     ((equal? name (normalize "isbn"))		id-isbn-name)
     ((equal? name (normalize "legalnotice"))	id-legalnotice-name)
     ((equal? name (normalize "msgaud"))	id-msgaud-name)
     ((equal? name (normalize "msglevel"))	id-msglevel-name)
     ((equal? name (normalize "msgorig"))	id-msgorig-name)
     ((equal? name (normalize "note"))		id-note-name)
     ((equal? name (normalize "part"))		id-part-name)
     ((equal? name (normalize "preface"))	id-preface-name)
     ((equal? name (normalize "procedure"))	id-procedure-name)
     ((equal? name (normalize "pubdate"))	id-pubdate-name)
     ((equal? name (normalize "question"))	id-question-name)
     ((equal? name (normalize "refentry"))	id-refentry-name)
     ((equal? name (normalize "reference"))	id-reference-name)
     ((equal? name (normalize "refname"))	id-refname-name)
     ((equal? name (normalize "revhistory"))	id-revhistory-name)
     ((equal? name (normalize "refsect1"))	id-refsect1-name)
     ((equal? name (normalize "refsect2"))	id-refsect2-name)
     ((equal? name (normalize "refsect3"))	id-refsect3-name)
     ((equal? name (normalize "refsynopsisdiv"))	id-refsynopsisdiv-name)
     ((equal? name (normalize "revision"))	id-revision-name)
     ((equal? name (normalize "sect1"))		id-sect1-name)
     ((equal? name (normalize "sect2"))		id-sect2-name)
     ((equal? name (normalize "sect3"))		id-sect3-name)
     ((equal? name (normalize "sect4"))		id-sect4-name)
     ((equal? name (normalize "sect5"))		id-sect5-name)
     ((equal? name (normalize "section"))	id-section-name)
     ((equal? name (normalize "simplesect"))    id-simplesect-name)
     ((equal? name (normalize "seeie"))		id-seeie-name)
     ((equal? name (normalize "seealsoie"))	id-seealsoie-name)
     ((equal? name (normalize "set"))		id-set-name)
     ((equal? name (normalize "sidebar"))	id-sidebar-name)
     ((equal? name (normalize "step"))		id-step-name)
     ((equal? name (normalize "table"))		id-table-name)
     ((equal? name (normalize "tip"))		id-tip-name)
     ((equal? name (normalize "toc"))		id-toc-name)
     ((equal? name (normalize "warning"))	id-warning-name)
     (else (let* ((msg (string-append "gentext-id-element-name: &unexpectedelementname;: " name))
		  (err (node-list-error msg (current-node))))
	     msg)))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-id-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;

(define id-equation-intra-label-sep "-")
(define id-informalequation-intra-label-sep "-")
(define id-example-intra-label-sep "-")
(define id-figure-intra-label-sep "-")
(define id-listitem-intra-label-sep ".")
(define id-procedure-intra-label-sep ".")
(define id-refentry-intra-label-sep ".")
(define id-reference-intra-label-sep ".")
(define id-refname-intra-label-sep ", ")
(define id-refsect1-intra-label-sep ".")
(define id-refsect2-intra-label-sep ".")
(define id-refsect3-intra-label-sep ".")
(define id-sect1-intra-label-sep ".")
(define id-sect2-intra-label-sep ".")
(define id-sect3-intra-label-sep ".")
(define id-sect4-intra-label-sep ".")
(define id-sect5-intra-label-sep ".")
(define id-section-intra-label-sep ".")
(define id-simplesect-intra-label-sep ".")
(define id-step-intra-label-sep ".")
(define id-table-intra-label-sep "-")
(define id-_pagenumber-intra-label-sep "-")
(define id-default-intra-label-sep "")

(define (gentext-id-intra-label-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "equation"))	id-equation-intra-label-sep)
     ((equal? name (normalize "informalequation"))	id-informalequation-intra-label-sep)
     ((equal? name (normalize "example"))	id-example-intra-label-sep)
     ((equal? name (normalize "figure"))	id-figure-intra-label-sep)
     ((equal? name (normalize "listitem"))	id-listitem-intra-label-sep)
     ((equal? name (normalize "procedure"))	id-procedure-intra-label-sep)
     ((equal? name (normalize "refentry"))	id-refentry-intra-label-sep)
     ((equal? name (normalize "reference"))	id-reference-intra-label-sep)
     ((equal? name (normalize "refname"))	id-refname-intra-label-sep)
     ((equal? name (normalize "refsect1"))	id-refsect1-intra-label-sep)
     ((equal? name (normalize "refsect2"))	id-refsect2-intra-label-sep)
     ((equal? name (normalize "refsect3"))	id-refsect3-intra-label-sep)
     ((equal? name (normalize "sect1"))		id-sect1-intra-label-sep)
     ((equal? name (normalize "sect2"))		id-sect2-intra-label-sep)
     ((equal? name (normalize "sect3"))		id-sect3-intra-label-sep)
     ((equal? name (normalize "sect4"))		id-sect4-intra-label-sep)
     ((equal? name (normalize "sect5"))		id-sect5-intra-label-sep)
     ((equal? name (normalize "section"))	id-section-intra-label-sep)
     ((equal? name (normalize "simplesect"))	id-simplesect-intra-label-sep)
     ((equal? name (normalize "step"))		id-step-intra-label-sep)
     ((equal? name (normalize "table"))		id-table-intra-label-sep)
     ((equal? name (normalize "_pagenumber"))	id-_pagenumber-intra-label-sep)
     (else id-default-intra-label-sep))))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;

(define id-abstract-label-title-sep ": ")
(define id-answer-label-title-sep " ")
(define id-appendix-label-title-sep ". ")
(define id-caution-label-title-sep "")
(define id-chapter-label-title-sep ". ")
(define id-equation-label-title-sep ". ")
(define id-example-label-title-sep ". ")
(define id-figure-label-title-sep ". ")
(define id-footnote-label-title-sep ". ")
(define id-glosssee-label-title-sep ": ")
(define id-glossseealso-label-title-sep ": ")
(define id-important-label-title-sep ": ")
(define id-note-label-title-sep ": ")
(define id-orderedlist-label-title-sep ". ")
(define id-part-label-title-sep ". ")
(define id-procedure-label-title-sep ". ")
(define id-prefix-label-title-sep ". ")
(define id-question-label-title-sep " ")
(define id-refentry-label-title-sep "")
(define id-reference-label-title-sep ". ")
(define id-refsect1-label-title-sep ". ")
(define id-refsect2-label-title-sep ". ")
(define id-refsect3-label-title-sep ". ")
(define id-sect1-label-title-sep ". ")
(define id-sect2-label-title-sep ". ")
(define id-sect3-label-title-sep ". ")
(define id-sect4-label-title-sep ". ")
(define id-sect5-label-title-sep ". ")
(define id-section-label-title-sep ". ")
(define id-simplesect-label-title-sep ". ")
(define id-seeie-label-title-sep " ")
(define id-seealsoie-label-title-sep " ")
(define id-step-label-title-sep ". ")
(define id-table-label-title-sep ". ")
(define id-tip-label-title-sep ": ")
(define id-warning-label-title-sep "")
(define id-default-label-title-sep "")

(define (gentext-id-label-title-sep gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "abstract")) id-abstract-label-title-sep)
     ((equal? name (normalize "answer")) id-answer-label-title-sep)
     ((equal? name (normalize "appendix")) id-appendix-label-title-sep)
     ((equal? name (normalize "caution")) id-caution-label-title-sep)
     ((equal? name (normalize "chapter")) id-chapter-label-title-sep)
     ((equal? name (normalize "equation")) id-equation-label-title-sep)
     ((equal? name (normalize "example")) id-example-label-title-sep)
     ((equal? name (normalize "figure")) id-figure-label-title-sep)
     ((equal? name (normalize "footnote")) id-footnote-label-title-sep)
     ((equal? name (normalize "glosssee")) id-glosssee-label-title-sep)
     ((equal? name (normalize "glossseealso")) id-glossseealso-label-title-sep)
     ((equal? name (normalize "important")) id-important-label-title-sep)
     ((equal? name (normalize "note")) id-note-label-title-sep)
     ((equal? name (normalize "orderedlist")) id-orderedlist-label-title-sep)
     ((equal? name (normalize "part")) id-part-label-title-sep)
     ((equal? name (normalize "procedure")) id-procedure-label-title-sep)
     ((equal? name (normalize "prefix")) id-prefix-label-title-sep)
     ((equal? name (normalize "question")) id-question-label-title-sep)
     ((equal? name (normalize "refentry")) id-refentry-label-title-sep)
     ((equal? name (normalize "reference")) id-reference-label-title-sep)
     ((equal? name (normalize "refsect1")) id-refsect1-label-title-sep)
     ((equal? name (normalize "refsect2")) id-refsect2-label-title-sep)
     ((equal? name (normalize "refsect3")) id-refsect3-label-title-sep)
     ((equal? name (normalize "sect1")) id-sect1-label-title-sep)
     ((equal? name (normalize "sect2")) id-sect2-label-title-sep)
     ((equal? name (normalize "sect3")) id-sect3-label-title-sep)
     ((equal? name (normalize "sect4")) id-sect4-label-title-sep)
     ((equal? name (normalize "sect5")) id-sect5-label-title-sep)
     ((equal? name (normalize "section")) id-section-label-title-sep)
     ((equal? name (normalize "simplesect")) id-simplesect-label-title-sep)
     ((equal? name (normalize "seeie")) id-seeie-label-title-sep)
     ((equal? name (normalize "seealsoie")) id-seealsoie-label-title-sep)
     ((equal? name (normalize "step")) id-step-label-title-sep)
     ((equal? name (normalize "table")) id-table-label-title-sep)
     ((equal? name (normalize "tip")) id-tip-label-title-sep)
     ((equal? name (normalize "warning")) id-warning-label-title-sep)
     (else id-default-label-title-sep))))

(define (id-set-label-number-format gind) "1")
(define (id-book-label-number-format gind) "1")
(define (id-prefix-label-number-format gind) "1")
(define (id-part-label-number-format gind) "I")
(define (id-chapter-label-number-format gind) "1")
(define (id-appendix-label-number-format gind) "A")
(define (id-reference-label-number-format gind) "I")
(define (id-example-label-number-format gind) "1")
(define (id-figure-label-number-format gind) "1")
(define (id-table-label-number-format gind) "1")
(define (id-procedure-label-number-format gind) "1")
(define (id-step-label-number-format gind) "1")
(define (id-refsect1-label-number-format gind) "1")
(define (id-refsect2-label-number-format gind) "1")
(define (id-refsect3-label-number-format gind) "1")
(define (id-sect1-label-number-format gind) "1")
(define (id-sect2-label-number-format gind) "1")
(define (id-sect3-label-number-format gind) "1")
(define (id-sect4-label-number-format gind) "1")
(define (id-sect5-label-number-format gind) "1")
(define (id-section-label-number-format gind) "1")
(define (id-default-label-number-format gind) "1")

(define (id-label-number-format gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond
     ((equal? name (normalize "set")) (id-set-label-number-format gind))
     ((equal? name (normalize "book")) (id-book-label-number-format gind))
     ((equal? name (normalize "prefix")) (id-prefix-label-number-format gind))
     ((equal? name (normalize "part")) (id-part-label-number-format gind))
     ((equal? name (normalize "chapter")) (id-chapter-label-number-format gind))
     ((equal? name (normalize "appendix")) (id-appendix-label-number-format gind))
     ((equal? name (normalize "reference")) (id-reference-label-number-format gind))
     ((equal? name (normalize "example")) (id-example-label-number-format gind))
     ((equal? name (normalize "figure")) (id-figure-label-number-format gind))
     ((equal? name (normalize "table")) (id-table-label-number-format gind))
     ((equal? name (normalize "procedure")) (id-procedure-label-number-format gind))
     ((equal? name (normalize "step")) (id-step-label-number-format gind))
     ((equal? name (normalize "refsect1")) (id-refsect1-label-number-format gind))
     ((equal? name (normalize "refsect2")) (id-refsect2-label-number-format gind))
     ((equal? name (normalize "refsect3")) (id-refsect3-label-number-format gind))
     ((equal? name (normalize "sect1")) (id-sect1-label-number-format gind))
     ((equal? name (normalize "sect2")) (id-sect2-label-number-format gind))
     ((equal? name (normalize "sect3")) (id-sect3-label-number-format gind))
     ((equal? name (normalize "sect4")) (id-sect4-label-number-format gind))
     ((equal? name (normalize "sect5")) (id-sect5-label-number-format gind))
     ((equal? name (normalize "section")) (id-section-label-number-format gind))
     (else (id-default-label-number-format gind)))))

(define ($lot-title-id$ gind)
  (let* ((giname (if (string? gind) gind (gi gind)))
	 (name   (normalize giname)))
    (cond ((equal? name (normalize "table"))    "&ListofTables;")
	  ((equal? name (normalize "example"))  "&ListofExamples;")
	  ((equal? name (normalize "figure"))   "&ListofFigures;")
	  ((equal? name (normalize "equation")) "&ListofEquations;")
	  (else (let* ((msg (string-append "&ListofUnknown;: " name))
		       (err (node-list-error msg (current-node))))
		  msg)))))

(define %gentext-id-start-quote% (dingbat "ldquo"))

(define %gentext-id-end-quote% (dingbat "rdquo"))

(define %gentext-id-start-nested-quote% (dingbat "lsquo"))

(define %gentext-id-end-nested-quote% (dingbat "rsquo"))

(define %gentext-id-by% "&by;") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-id-edited-by% "&Editedby;")
                           ;; "Edited by" Jane Doe

(define %gentext-id-revised-by% "&Revisedby;")
                           ;; "Revised by" Jane Doe

(define %gentext-id-page% "")

(define %gentext-id-and% "&and;")

(define %gentext-id-listcomma% "&listcomma;")

(define %gentext-id-lastlistcomma% "&lastlistcomma;")

(define %gentext-id-bibl-pages% "&Pgs;")

(define %gentext-id-endnotes% "&Notes;")

(define %gentext-id-table-endnotes% "&TableNotes;:")

(define %gentext-id-index-see% "&See;")

(define %gentext-id-index-seealso% "&SeeAlso;")


(define (gentext-id-nav-prev prev) 
  (make sequence (literal "&nav-prev;")))

(define (gentext-id-nav-prev-sibling prevsib) 
  (make sequence (literal "&nav-prev-sibling;")))

(define (gentext-id-nav-next-sibling nextsib)
  (make sequence (literal "&nav-next-sibling;")))

(define (gentext-id-nav-next next)
  (make sequence (literal "&nav-next;")))

(define (gentext-id-nav-up up)
  (make sequence (literal "&nav-up;")))

(define (gentext-id-nav-home home)
  (make sequence (literal "&nav-home;")))


</style-specification-body>
</style-specification>
</style-sheet>
