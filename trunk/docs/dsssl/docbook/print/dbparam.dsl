<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">

<style-sheet>
<style-specification>
<style-specification-body>

;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; === Book intro, for dsl2man ==========================================

<![CDATA[
;; DOCINFO
;; <title>DocBook Print Parameters</title>
;; <subtitle>Part of the Modular DocBook Stylesheet distribution</subtitle>
;; <author><firstname>Norman</firstname><surname>Walsh</surname>
;; </author>
;; <edition>$Revision$</edition>
;; <copyright><year>1997</year><year>1998</year><year>1999</year>
;; <holder>Norman Walsh</holder></copyright>
;; <legalnotice>
;; <para>
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL NORMAN WALSH OR ANY OTHER
;; CONTRIBUTOR BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;; </para>
;; </legalnotice>
;; <legalnotice>
;; <para>
;; Please direct all questions, bug reports, or suggestions for changes
;; to Norman Walsh, &lt;<literal>ndw@nwalsh.com</literal>&gt;.
;; </para>
;; <para>
;; See <ulink url="http://nwalsh.com/docbook/dsssl/">http://nwalsh.com/docbook/dsssl/</ulink> for more information.</para>
;; </legalnotice>
;; /DOCINFO
]]>

;; REFERENCE TOC/LOT Apparatus

(define %generate-set-toc% 
  ;; REFENTRY generate-set-toc
  ;; PURP Should a Table of Contents be produced for Sets?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Set'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-book-toc% 
  ;; REFENTRY generate-book-toc
  ;; PURP Should a Table of Contents be produced for Books?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Book'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define ($generate-book-lot-list$)
  ;; REFENTRY generate-book-lot-list
  ;; PURP Which Lists of Titles should be produced for Books?
  ;; DESC
  ;; This parameter should be a list (possibly empty) of the elements
  ;; for which Lists of Titles should be produced for each 'Book'.
  ;;
  ;; It is meaningless to put elements that do not have titles in this
  ;; list.  If elements with optional titles are placed in this list, only
  ;; the instances of those elements that do have titles will appear in
  ;; the LOT.
  ;;
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (list (normalize "table")
(normalize "figure")
(normalize "example")
(normalize "equation")))

(define %generate-part-toc% 
  ;; REFENTRY generate-part-toc
  ;; PURP Should a Table of Contents be produced for Parts?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Part'.
  ;; Note: '%generate-part-toc-on-titlepage%' controls whether the Part TOC
  ;; is placed on the bottom of the part titlepage or on page(s) of its own.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-part-toc-on-titlepage%
  ;; REFENTRY generate-part-toc-on-titlepage
  ;; PURP Should the Part TOC appear on the Part title page?
  ;; DESC
  ;; If true, the Part TOC will be placed on the Part title page.  If false,
  ;; the TOC will be placed on separate page(s) after the Part title page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-reference-toc% 
  ;; REFENTRY generate-reference-toc
  ;; PURP Should a Table of Contents be produced for References?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Reference'.
  ;; Note: '%generate-reference-toc-on-titlepage%' controls whether the
  ;; Reference TOC
  ;; is placed on the bottom of the title page or on page(s) of its own.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-reference-toc-on-titlepage%
  ;; REFENTRY generate-reference-toc-on-titlepage
  ;; PURP Should the Reference TOC appear on the Reference title page?
  ;; DESC
  ;; If true, the Reference TOC will be placed on the Reference title page.
  ;; If false,
  ;; the TOC will be placed on separate page(s) after the title page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-article-toc% 
  ;; REFENTRY generate-article-toc
  ;; PURP Should a Table of Contents be produced for Articles?
  ;; DESC
  ;; If true, a Table of Contents will be generated for each 'Article'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-article-toc-on-titlepage%
  ;; REFENTRY generate-article-toc-on-titlepage
  ;; PURP Should the Article TOC appear on the Article title page?
  ;; DESC
  ;; If true, the Article TOC will be placed on the Article title page.
  ;; If false,
  ;; the TOC will be placed on separate page(s) after the title page.
  ;; If false, %generate-article-titlepage-on-separate-page% should be
  ;; true.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; REFERENCE Titlepages

(define %generate-set-titlepage%
  ;; REFENTRY generate-set-titlepage
  ;; PURP Should a set title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Set'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-book-titlepage%
  ;; REFENTRY generate-book-titlepage
  ;; PURP Should a book title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Book'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-part-titlepage% 
  ;; REFENTRY generate-part-titlepage
  ;; PURP Should a part title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Part'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-partintro-on-titlepage%
  ;; REFENTRY generate-partintro-on-titlepage
  ;; PURP Should the PartIntro appear on the Part/Reference title page?
  ;; DESC
  ;; If true, the PartIntro content will appear on the title page of
  ;; Parts and References.  If false,
  ;; it will be placed on separate page(s) after the title page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-reference-titlepage% 
  ;; REFENTRY generate-reference-titlepage
  ;; PURP Should a reference title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Reference'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-article-titlepage% 
  ;; REFENTRY generate-article-titlepage
  ;; PURP Should an article title page be produced?
  ;; DESC
  ;; If true, a title page will be generated for each 'Article'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %generate-article-titlepage-on-separate-page%
  ;; REFENTRY generate-article-ttlpg-on-sep-page
  ;; PURP Should the article title page be on a separate page?
  ;; DESC
  ;; If true, the title page for each 'Article' will occur on its own page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %titlepage-in-info-order% 
  ;; REFENTRY titlepage-in-info-order
  ;; PURP Place elements on title page in document order?
  ;; DESC
  ;; If true, the elements on the title page will be set in the order that
  ;; they appear in the *info element.  Otherwise, they will be set in
  ;; the order specified in the *-titlepage-*-elements list.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %author-othername-in-middle%
  ;; REFENTRY othername-in-middle
  ;; PURP Author OTHERNAME appears between FIRSTNAME and SURNAME?
  ;; DESC
  ;; If true, the OTHERNAME of an AUTHOR appears between the 
  ;; FIRSTNAME and SURNAME.  Otherwise, OTHERNAME is suppressed.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; REFERENCE RefEntries and FuncSynopses

(define %refentry-new-page% 
  ;; REFENTRY refentry-new-page
  ;; PURP 'RefEntry' starts on new page?
  ;; DESC
  ;; If true, each 'RefEntry' begins on a new page.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %refentry-keep% 
  ;; REFENTRY refentry-keep
  ;; PURP Keep RefEntrys together?
  ;; DESC
  ;; Refentry keep indicates how the stylesheet should
  ;; attempt to keep each RefEntry.  Common values are '#t', for the
  ;; smallest possible area, 'page' for the same page, and '#f' to ignore
  ;; this characteristic.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %refentry-generate-name% 
  ;; REFENTRY refentry-generate-name
  ;; PURP Output NAME header before 'RefName'(s)?
  ;; DESC
  ;; If true, a "NAME" section title is output before the list
  ;; of 'RefName's.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %refentry-xref-italic%
  ;; REFENTRY refentry-xref-italic
  ;; PURP Use italic text when cross-referencing RefEntrys?
  ;; DESC
  ;; If true, italics are used when cross-referencing RefEntrys, either
  ;; with XRef or CiteRefEntry.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %refentry-xref-manvolnum%
  ;; REFENTRY refentry-xref-manvolnum
  ;; PURP Output manvolnum as part of RefEntry cross-reference?
  ;; DESC
  ;; If true, the manvolnum is used when cross-referencing RefEntrys, either
  ;; with XRef or CiteRefEntry.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %funcsynopsis-style% 
  ;; REFENTRY funcsynopsis-style
  ;; PURP What style of 'FuncSynopsis' should be generated?
  ;; DESC
  ;; If '%funcsynopsis-style%' is 'ansi',
  ;; ANSI-style function synopses are generated for a 'FuncSynopsis',
  ;; otherwise K<![CDATA[&]]>R-style function synopses are generated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'ansi)

(define %kr-funcsynopsis-indent% 
  ;; REFENTRY kr-funcsynopsis-indent
  ;; PURP Indent-depth in K<![CDATA[&]]>R-style function synopses
  ;; DESC
  ;; If the '%funcsynopsis-style%' is 'kr',
  ;; '%kr-funcsynopsis-indent%' specifies the amount by which parameter
  ;; definitions should be indented under the function prototype.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1pi)

(define %funcsynopsis-decoration%
  ;; REFENTRY funcsynopsis-decoration
  ;; PURP Decorate elements of a FuncSynopsis?
  ;; DESC
  ;; If true, elements of the FuncSynopsis will be decorated (e.g. bold or
  ;; italic).  The decoration is controlled by functions that can be redefined
  ;; in a customization layer.  See 'edbsynop.dsl'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; REFERENCE Fonts

(define %refentry-name-font-family%
  ;; REFENTRY refentry-name-font-family
  ;; PURP The font family used in RefName
  ;; DESC
  ;; The name of the font family used in 'RefEntry'
  ;; 'RefName's.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  %mono-font-family%)

(define %title-font-family% 
  ;; REFENTRY title-font-family
  ;; PURP The font family used in titles
  ;; DESC
  ;; The name of the font family used in titles (Arial by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Arial")

(define %body-font-family% 
  ;; REFENTRY body-font-family
  ;; PURP The font family used in body text
  ;; DESC
  ;; The name of the font family used in body text 
  ;; (Times New Roman by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Times New Roman")

(define %mono-font-family% 
  ;; REFENTRY mono-font-family
  ;; PURP The font family used in verbatim environments
  ;; DESC
  ;; The name of the font family used in verbatim environments (Courier New
  ;; by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Courier New")

(define %admon-font-family% 
  ;; REFENTRY admon-font-family
  ;; PURP The font family used in admonitions
  ;; DESC
  ;; The name of the font family used for body text in admonitions (Arial
  ;; by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Arial")

(define %guilabel-font-family%
  ;; REFENTRY guilabel-font-family
  ;; PURP The font family used in GUI labels
  ;; DESC
  ;; The name of the font family used for text that represents text on a
  ;; GUI (e.g., text in 'GUILabel', 'GUIMenu',
  ;; etc.). (Arial by default).
  ;;
  ;; The values used here are system dependent (you have
  ;; to have the fonts you select) and backend dependent (the backend has
  ;; to know how to use them). 
  ;;
  ;; The values here work for the RTF backend under MS Windows.  YMMV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "Arial")

(define %visual-acuity%
  ;; REFENTRY visual-acuity
  ;; PURP General measure of document text size
  ;; DESC
  ;; This parameter controls the general size of the text in the document.
  ;; Several other values (body font size and margins) have default values that
  ;; vary depending on the setting of '%visual-acuity%'. There
  ;; are three legal values: 'normal', 
  ;; the normal, standard document size (10pt body text);
  ;; 'presbyopic', 
  ;; a slightly more generous size (12pt body text); and
  ;; 'large-type',
  ;; quite large (24pt body text).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ;; "presbyopic"
  ;; "large-type"
  "normal")

(define %hsize-bump-factor% 
  ;; REFENTRY hsize-bump-factor
  ;; PURP Font scaling factor
  ;; DESC
  ;; Internally, the stylesheet refers to font sizes in purely relative
  ;; terms. This is done by defining a scaled set of fonts 
  ;; (sizes 1, 2, 3, etc.)
  ;; based at the default text font size (e.g. 10pt). The '%hsize-bump-factor%'
  ;; describes the ratio between scaled sizes. The default is 1.2.
  ;;
  ;; Each hsize is '%hsize-bump-factor%' times larger than
  ;; the previous hsize. For example, if the base size is 10pt, and 
  ;; '%hsize-bump-factor%'
  ;; 1.2, hsize 1 is 12pt, hsize 2 is 14.4pt, hsize 3 is 17.28pt, etc.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1.2)

(define %smaller-size-factor% 
  ;; REFENTRY smaller-size-factor
  ;; PURP Smaller font scaling factor
  ;; DESC
  ;; In environments that are usually set with a slightly smaller font size,
  ;; for example block quotations, the stylesheet calculates the smaller font
  ;; size by muliplying the current font size by '%smaller-size-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.9)

(define %ss-size-factor% 
  ;; REFENTRY ss-size-factor
  ;; PURP Super/subscript scaling factor
  ;; DESC
  ;; When text is set as a subscript or superscript, the font size of the
  ;; text is multiplied by '%ss-size-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.6)

(define %ss-shift-factor%
  ;; REFENTRY ss-shift-factor
  ;; PURP Super/subscript shift factor
  ;; DESC
  ;; When text is set as a subscript or superscript, it is set above or below
  ;; the baseline by a factor of the current font size and '%ss-shift-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.4)

(define %verbatim-size-factor% 
  ;; REFENTRY verbatim-size-factor
  ;; PURP Verbatim font scaling factor
  ;; DESC
  ;; When a monospace font is selected, the current font size is multiplied
  ;; by the '%verbatim-size-factor%'. If '%verbatim-size-factor%'
  ;; is '#f', no scaling is performed (Well, that's not precisely true. 
  ;; In '$verbatim-display$'
  ;; environments, the font size is calculated with respect to the longest line
  ;; in the display, if '%verbatim-size-factor%' is '#f').
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.9)

(define %bf-size%
  ;; REFENTRY bf-size
  ;; PURP Defines the body font size
  ;; DESC
  ;; Sets the body font size. This parameter is usually controlled by the
  ;; '%visual-acuity%' parameter.
  ;; /DESC
  ;; /REFENTRY
  (case %visual-acuity%
    (("tiny") 8pt)
    (("normal") 10pt)
    (("presbyopic") 12pt)
    (("large-type") 24pt)))

(define-unit em %bf-size%)

(define %footnote-size-factor% 
  ;; REFENTRY footnote-size-factor
  ;; PURP Footnote font scaling factor
  ;; DESC
  ;; When printing footnotes, the current font size is multiplied by the
  ;; '%footnote-size-factor%'. 
  ;; /DESC
  ;; /REFENTRY
  0.9)

;; REFERENCE Backends

(define tex-backend 
  ;; REFENTRY tex-backend
  ;; PURP Are we using the TeX backend?
  ;; DESC
  ;; This parameter exists so that '-V tex-backend' can be used on the
  ;; command line to explicitly select the TeX backend.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define mif-backend 
  ;; REFENTRY mif-backend
  ;; PURP Are we using the MIF backend?
  ;; DESC
  ;; This parameter exists so that '-V mif-backend' can be used on the
  ;; command line to explicitly select the MIF backend.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define rtf-backend 
  ;; REFENTRY rtf-backend
  ;; PURP Are we using the RTF backend?
  ;; DESC
  ;; This parameter exists so that '-V rtf-backend' can be used on the
  ;; command line to explicitly select the RTF backend.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define default-backend 
  ;; REFENTRY default-backend
  ;; PURP What is the default backend?
  ;; DESC
  ;; This parameter sets the default backend.  Selecting an explicit
  ;; backend enables features specific to that backend (if there are any).
  ;; The legal values are 'rtf', 'tex', 'mif', and '#f'. Using
  ;; '#f' implies that no special features are used.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define (print-backend)
  ;; REFENTRY print-backend
  ;; PURP Returns the backend that is being used to format the document
  ;; DESC
  ;; This parameter controls features in the stylesheet that are backend
  ;; specific.  The legal values are 'rtf', 'tex', 'mif', and '#f'. Using
  ;; '#f' implies that no special features are used.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (cond
   (tex-backend 'tex)
   (mif-backend 'mif)
   (rtf-backend 'rtf)
   (else default-backend)))

;; REFERENCE Verbatim Environments

(define %verbatim-default-width%
  ;; REFENTRY verbatim-default-width
  ;; PURP Default width of verbatim environments
  ;; DESC
  ;; If no WIDTH attribute is specified on verbatim environments, 
  ;; '%verbatim-default-width%' is the default.  Note: this width only
  ;; comes into play if '%verbatim-size-factor%' is '#f'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  80)

(define %number-synopsis-lines% 
  ;; REFENTRY number-synopsis-lines
  ;; PURP Enumerate lines in a 'Synopsis'?
  ;; DESC
  ;; If true, lines in each 'Synopsis' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%', 
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-funcsynopsisinfo-lines% 
  ;; REFENTRY number-funcsynopsisinfo-lines
  ;; PURP Enumerate lines in a 'FuncSynopsisInfo'?
  ;; DESC
  ;; If true, lines in each 'FuncSynopsisInfo' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-literallayout-lines% 
  ;; REFENTRY number-literallayout-lines
  ;; PURP Enumerate lines in a 'LiteralLayout'?
  ;; DESC
  ;; If true, lines in each 'LiteralLayout' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-address-lines% 
  ;; REFENTRY number-address-lines
  ;; PURP Enumerate lines in a 'Address'?
  ;; DESC
  ;; If true, lines in each 'Address' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-programlisting-lines%
  ;; REFENTRY number-programlisting-lines
  ;; PURP Enumerate lines in a 'ProgramListing'?
  ;; DESC
  ;; If true, lines in each 'ProgramListing' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %number-screen-lines%
  ;; REFENTRY number-screen-lines
  ;; PURP Enumerate lines in a 'Screen'?
  ;; DESC
  ;; If true, lines in each 'Screen' will be enumerated.
  ;; See also '%linenumber-mod%', '%linenumber-length%',
  ;; '%linenumber-padchar%', and '($linenumber-space$)'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %linenumber-mod% 
  ;; REFENTRY linenumber-mod
  ;; PURP Controls line-number frequency in enumerated environments.
  ;; DESC
  ;; Every '%linenumber-mod%' line will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  5)

(define %linenumber-length% 
  ;; REFENTRY linenumber-length
  ;; PURP Width of line numbers in enumerated environments
  ;; DESC
  ;; Line numbers will be padded to '%linenumber-length%'
  ;; characters.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  3)

(define %linenumber-padchar% 
  ;; REFENTRY linenumber-padchar
  ;; PURP Pad character in line numbers
  ;; DESC
  ;; Line numbers will be padded (on the left) with '%linenumber-padchar%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "\no-break-space;")

(define ($linenumber-space$) 
  ;; REFENTRY linenumber-space
  ;; PURP Returns the sosofo which separates line numbers from the text
  ;; DESC
  ;; The sosofo returned by '($linenumber-space$)' is placed
  ;; between the line number and the corresponding line in 
  ;; enumerated environments.
  ;;
  ;; Note: '%linenumber-padchar%'s are separated from lines
  ;; that are not enumerated (because they don't match '%linenumber-mod%').
  ;; In other words, '($linenumber-space$)' occurs
  ;; on every line.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (literal "\no-break-space;"))

(define %indent-synopsis-lines% 
  ;; REFENTRY indent-synopsis-lines
  ;; PURP Indent lines in a 'Synopsis'?
  ;; DESC
  ;; If not '#f', each line in the display will be indented
  ;; with the content of this variable.  Usually it is set to some number
  ;; of spaces, but you can indent with any string you wish.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %indent-funcsynopsisinfo-lines% 
  ;; REFENTRY indent-funcsynopsisinfo-lines
  ;; PURP Indent lines in a 'FuncSynopsisInfo'?
  ;; DESC
  ;; If not '#f', each line in the display will be indented
  ;; with the content of this variable.  Usually it is set to some number
  ;; of spaces, but you can indent with any string you wish.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %indent-literallayout-lines% 
  ;; REFENTRY indent-literallayout-lines
  ;; PURP Indent lines in a 'LiteralLayout'?
  ;; DESC
  ;; If not '#f', each line in the display will be indented
  ;; with the content of this variable.  Usually it is set to some number
  ;; of spaces, but you can indent with any string you wish.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %indent-address-lines% 
  ;; REFENTRY indent-address-lines
  ;; PURP Indent lines in a 'Address'?
  ;; DESC
  ;; If not '#f', each line in the display will be indented
  ;; with the content of this variable.  Usually it is set to some number
  ;; of spaces, but you can indent with any string you wish.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %indent-programlisting-lines%
  ;; REFENTRY indent-programlisting-lines
  ;; PURP Indent lines in a 'ProgramListing'?
  ;; DESC
  ;; If not '#f', each line in the display will be indented
  ;; with the content of this variable.  Usually it is set to some number
  ;; of spaces, but you can indent with any string you wish.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %indent-screen-lines%
  ;; REFENTRY indent-screen-lines
  ;; PURP Indent lines in a 'Screen'?
  ;; DESC
  ;; If not '#f', each line in the display will be indented
  ;; with the content of this variable.  Usually it is set to some number
  ;; of spaces, but you can indent with any string you wish.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %callout-fancy-bug% 
  ;; REFENTRY callout-fancy-bug
  ;; PURP Use fancy callout bugs?
  ;; DESC
  ;; If true, fancy callout bugs will be used. Otherwise, simple ones are
  ;; used. Fancy callout bugs may require the RTF backend.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %callout-default-col% 
  ;; REFENTRY callout-default-col
  ;; PURP Default column for callouts
  ;; DESC
  ;; If the coordinates of a callout include only a line number, the callout
  ;; bug will appear in column '%callout-default-col%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  60)

;; REFERENCE Labelling

(define %section-autolabel% 
  ;; REFENTRY section-autolabel
  ;; PURP Are sections enumerated?
  ;; DESC
  ;; If true, unlabeled sections will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %chapter-autolabel% 
  ;; REFENTRY chapter-autolabel
  ;; PURP Are chapters enumerated?
  ;; DESC
  ;; If true, chapters will be enumerated.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %label-preface-sections%
  ;; REFENTRY label-preface-sections
  ;; PURP Are sections in the Preface enumerated?
  ;; DESC
  ;; If true, unlabeled sections in the Preface will be enumerated
  ;; if '%section-autolabel%' is true.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %qanda-inherit-numeration% 
  ;; REFENTRY qanda-inherit-numeration
  ;; PURP Should numbered questions inherit the surrounding numeration?
  ;; DESC
  ;; If true, question numbers are prefixed with the surrounding
  ;; component or section number. Has no effect unless
  ;; '%section-autolabel%' is also true.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

;; REFERENCE Running Heads

(define %chap-app-running-heads% 
  ;; REFENTRY chap-app-running-heads
  ;; PURP Generate running headers and footers on chapter-level elements?
  ;; DESC
  ;; If true, running headers and footers are produced on chapter-level 
  ;; elements.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %chap-app-running-head-autolabel% 
  ;; REFENTRY chap-app-running-head-autolabel
  ;; PURP Put chapter labels in running heads?
  ;; DESC
  ;; If true, running heads on 'Chapter's and 
  ;; 'Appendix'es will include an automatic label.
  ;; 
  ;; In other words, if a 'Chapter' has no 'Label' attribute,
  ;; and '%chap-app-running-head-autolabel%'
  ;; is true, running heads will include the automatic label for the
  ;; 'Chapter'. If '%chap-app-running-head-autolabel%'
  ;; is false, only the 'Title' (or 'TitleAbbrev')
  ;; will appear in the running head.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; REFERENCE Paper/Page Characteristics

(define %paper-type%
  ;; REFENTRY paper-type
  ;; PURP Name of paper type
  ;; DESC
  ;; The paper type value identifies the sort of paper in use, for example, 
  ;; 'A4' or 'USletter'. Setting the paper type is an
  ;; easy shortcut for setting the correct paper height and width.
  ;; 
  ;; As distributed, only 'A4' and 'USletter' are supported.  You can add
  ;; additional paper types by updating 'page-width' and 'page-height'.
  ;; If you do, please pass along your updates. 
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ;; "A4"
  "USletter")

(define %two-side% 
  ;; REFENTRY two-side
  ;; PURP Is two-sided output being produced?
  ;; DESC
  ;; If '%two-side%' is true, headers and footers are alternated
  ;; so that the "outer" and "inner" headers will be correctly
  ;; placed in the bound document.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %writing-mode%
  ;; REFENTRY writing-mode
  ;; PURP The writing mode
  ;; DESC
  ;; The writing mode is either 'left-to-right', or 
  ;; 'right-to-left'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'left-to-right)

(define %page-n-columns%
  ;; REFENTRY page-n-columns
  ;; PURP Sets the number of columns on each page
  ;; DESC
  ;; Sets the number of columns on each page
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1)

(define %titlepage-n-columns%
  ;; REFENTRY titlepage-n-columns
  ;; PURP Sets the number of columns on the title page
  ;; DESC
  ;; Sets the number of columns on the title page
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1)

(define %page-column-sep%
  ;; REFENTRY page-column-sep
  ;; PURP Sets the width of the gutter between columns
  ;; DESC
  ;; Sets the width of the gutter between columns
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.5in)

(define %page-balance-columns?% 
  ;; REFENTRY page-balance-columns
  ;; PURP Balance columns on pages?
  ;; DESC
  ;; If true, the columns on the final page of a multiple column layout
  ;; will be balanced.  Otherwise, the columns will be completely filled in the
  ;; writing direction and the last column may be a different length 
  ;; than the preceding columns.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %left-margin% 
  ;; REFENTRY left-margin
  ;; PURP Width of left margin
  ;; DESC
  ;; The '%left-margin%' parameter specifies the width of the left margin
  ;; of the page.  Note that this setting is relative to the physical page,
  ;; not the writing direction.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  6pi)

(define %right-margin% 
  ;; REFENTRY right-margin
  ;; PURP Width of the right margin
  ;; DESC
  ;; The '%right-margin%' parameter specifies the width of the right margin
  ;; of the page.  Note that this setting is relative to the physical page,
  ;; not the writing direction.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  6pi)

(define %page-width%
  ;; REFENTRY page-width
  ;; PURP Specifies the page width
  ;; DESC
  ;; Identifies the width of the page (length in the writing direction).
  ;; It is usually controlled by the '%paper-type%' parameter.
  ;; /DESC
  ;; /REFENTRY
  (case %paper-type%
    (("A4landscape") 297mm)
    (("USletter") 8.5in)
    (("USlandscape") 11in)
    (("4A0") 1682mm)
    (("2A0") 1189mm)
    (("A0") 841mm)
    (("A1") 594mm)
    (("A2") 420mm)
    (("A3") 297mm)
    (("A4") 210mm)
    (("A5") 148mm)
    (("A6") 105mm)
    (("A7") 74mm)
    (("A8") 52mm)
    (("A9") 37mm)
    (("A10") 26mm)
    (("B0") 1000mm)
    (("B1") 707mm)
    (("B2") 500mm)
    (("B3") 353mm)
    (("B4") 250mm)
    (("B5") 176mm)
    (("B6") 125mm)
    (("B7") 88mm)
    (("B8") 62mm)
    (("B9") 44mm)
    (("B10") 31mm)
    (("C0") 917mm)
    (("C1") 648mm)
    (("C2") 458mm)
    (("C3") 324mm)
    (("C4") 229mm)
    (("C5") 162mm)
    (("C6") 114mm)
    (("C7") 81mm)
    (("C8") 57mm)
    (("C9") 40mm)
    (("C10") 28mm)))

(define %page-height%
  ;; REFENTRY page-height
  ;; PURP Specifies the page height
  ;; DESC
  ;; Identifies the height of the page (length perpendicular to the
  ;; writing direction).
  ;; It is usually controlled by the '%paper-type%' parameter.
  ;; /DESC
  ;; /REFENTRY
  (case %paper-type%
    (("A4landscape") 210mm)
    (("USletter") 11in)
    (("USlandscape") 8.5in)
    (("4A0") 2378mm)
    (("2A0") 1682mm)
    (("A0") 1189mm)
    (("A1") 841mm)
    (("A2") 594mm)
    (("A3") 420mm)
    (("A4") 297mm)
    (("A5") 210mm)
    (("A6") 148mm)
    (("A7") 105mm)
    (("A8") 74mm)
    (("A9") 52mm)
    (("A10") 37mm)
    (("B0") 1414mm)
    (("B1") 1000mm)
    (("B2") 707mm)
    (("B3") 500mm)
    (("B4") 353mm)
    (("B5") 250mm)
    (("B6") 176mm)
    (("B7") 125mm)
    (("B8") 88mm)
    (("B9") 62mm)
    (("B10") 44mm)
    (("C0") 1297mm)
    (("C1") 917mm)
    (("C2") 648mm)
    (("C3") 458mm)
    (("C4") 324mm)
    (("C5") 229mm)
    (("C6") 162mm)
    (("C7") 114mm)
    (("C8") 81mm)
    (("C9") 57mm)
    (("C10") 40mm)))

(define %text-width% 
  ;; REFENTRY text-width
  ;; PURP Specifies the width of the body column
  ;; DESC
  ;; Identifies the width of the page on which text may occur.
  ;; /DESC
  ;; /REFENTRY
  (- %page-width% (+ %left-margin% %right-margin%)))

(define %body-width% 
  ;; REFENTRY body-width
  ;; PURP Specifies the width of the text in the body column
  ;; DESC
  ;; Identifies the width of the page on which text will occur, after
  ;; the '%body-start-indent%' is removed.
  ;; /DESC
  ;; /REFENTRY
  (- %text-width% %body-start-indent%))

(define %top-margin% 
  ;; REFENTRY top-margin
  ;; PURP Height of top margin
  ;; DESC
  ;; The '%top-margin%' parameter specifies the height of the
  ;; top margin
  ;; of the page.  Note that this setting is relative to the physical page,
  ;; not the writing direction.
  ;; /DESC
  ;; /REFENTRY
  (if (equal? %visual-acuity% "large-type")
      7.5pi
      6pi))

(define %bottom-margin% 
  ;; REFENTRY bottom-margin
  ;; PURP Height of bottom margin
  ;; DESC
  ;; The '%bottom-margin%' parameter specifies the
  ;; height of the bottom margin
  ;; of the page.  Note that this setting is relative to the physical page,
  ;; not the writing direction.
  ;; /DESC
  ;; /REFENTRY
  (if (equal? %visual-acuity% "large-type")
      9.5pi 
      8pi))

(define %header-margin% 
  ;; REFENTRY header-margin
  ;; PURP Height of header margin
  ;; DESC
  ;; The '%header-margin%' parameter specifies the heigth
  ;; of the header margin
  ;; of the page.  Note that this setting is relative to the physical page,
  ;; not the writing direction.
  ;; /DESC
  ;; /REFENTRY
  (if (equal? %visual-acuity% "large-type") 
      5.5pi 
      4pi))

(define %footer-margin% 
  ;; REFENTRY footer-margin
  ;; PURP Height of footer margin
  ;; DESC
  ;; The '%footer-margin%' parameter specifies the height
  ;; of the footer margin
  ;; of the page.  Note that this setting is relative to the physical page,
  ;; not the writing direction.
  ;; /DESC
  ;; /REFENTRY
  4pi)

(define %page-number-restart% 
  ;; REFENTRY page-number-restart
  ;; PURP Restart page numbers in each component?
  ;; DESC
  ;; If true, page numbers are restarted at the beginning of each 
  ;; component-level
  ;; element ('Chapter', 'Appendix',
  ;; 'Bibliography', etc.).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %article-page-number-restart% 
  ;; REFENTRY article-page-number-restart
  ;; PURP Restart page numbers in each article?
  ;; DESC
  ;; If true, page numbers are restarted at the beginning of each 
  ;; article.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %generate-heading-level%   
  ;; REFENTRY generate-heading-level
  ;; PURP Output RTF heading level characteristics?
  ;; DESC
  ;; If true, component and section titles will have the heading-level
  ;; characteristic in the RTF.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

;; REFERENCE Admonitions

(define %admon-graphics%
  ;; REFENTRY admon-graphics
  ;; PURP Use graphics in admonitions?
  ;; DESC
  ;; If true, admonitions are presented in an alternate style that uses
  ;; a graphic.  Default graphics are provided in the distribution.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %admon-graphics-path%
  ;; REFENTRY admon-graphics-path
  ;; PURP Path to admonition graphics
  ;; DESC
  ;; Sets the path, probably relative to the directory where the HTML
  ;; files are created, to the admonition graphics.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "../images/")

(define admon-graphic-default-extension
  ;; REFENTRY admon-graphic-default-extension
  ;; PURP Admonition graphic file extension
  ;; DESC
  ;; Identifies the default extension for admonition graphics. This allows
  ;; backends to select different images (e.g., EPS for print, PNG for
  ;; PDF, etc.)
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".eps")

(define ($admon-graphic$ #!optional (nd (current-node)))
  ;; REFENTRY admon-graphic
  ;; PURP Admonition graphic file
  ;; DESC
  ;; Given an admonition node, returns the name of the graphic that should
  ;; be used for that admonition.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (cond ((equal? (gi nd) (normalize "tip"))
	 (string-append %admon-graphics-path%
			(string-append "tip"
				       admon-graphic-default-extension)))
	((equal? (gi nd) (normalize "note"))
	 (string-append %admon-graphics-path%
			(string-append "note"
				       admon-graphic-default-extension)))

	((equal? (gi nd) (normalize "important"))
	 (string-append %admon-graphics-path%
			(string-append "important"
				       admon-graphic-default-extension)))

	((equal? (gi nd) (normalize "caution"))
	 (string-append %admon-graphics-path%
			(string-append "caution"
				       admon-graphic-default-extension)))
	((equal? (gi nd) (normalize "warning"))
	 (string-append %admon-graphics-path%
			(string-append "warning"
				       admon-graphic-default-extension)))
	(else (error (string-append (gi nd) " is not an admonition.")))))

(define ($admon-graphic-width$ #!optional (nd (current-node)))
  ;; REFENTRY admon-graphic-width
  ;; PURP Admonition graphic file width
  ;; DESC
  ;; Given an admonition node, returns the width of the graphic that will
  ;; be used for that admonition.
  ;;
  ;; All of the default graphics in the distribution are 0.3in wide.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.3in)

;; REFERENCE Quadding

(define %default-quadding%   
  ;; REFENTRY default-quadding
  ;; PURP The default quadding
  ;; DESC
  ;; The default quadding ('start', 'center', 'justify',
  ;; or 'end').
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %division-title-quadding% 
  ;; REFENTRY division-title-quadding
  ;; PURP Division title quadding
  ;; DESC
  ;; The quadding of division-level titles ('Set', 'Book', and 'Part').
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'center)

(define %division-subtitle-quadding% 
  ;; REFENTRY division-subtitle-quadding
  ;; PURP Division subtitle quadding
  ;; DESC
  ;; The quadding of division-level subtitles ('Set', 'Book', and 'Part').
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'center)

(define %component-title-quadding% 
  ;; REFENTRY component-title-quadding
  ;; PURP Component title quadding
  ;; DESC
  ;; The quadding of component-level titles ('Chapter',
  ;; 'Appendix', 'Glossary', etc.).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %component-subtitle-quadding% 
  ;; REFENTRY component-subtitle-quadding
  ;; PURP Component subtitle quadding
  ;; DESC
  ;; The quadding of component-level subtitles ('Chapter',
  ;; 'Appendix', 'Glossary', etc.).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %article-title-quadding% 
  ;; REFENTRY article-title-quadding
  ;; PURP Article title quadding
  ;; DESC
  ;; The quadding of article titles.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'center)

(define %article-subtitle-quadding% 
  ;; REFENTRY article-subtitle-quadding
  ;; PURP Article subtitle quadding
  ;; DESC
  ;; The quadding of article subtitles.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'center)

(define %section-title-quadding% 
  ;; REFENTRY section-title-quadding
  ;; PURP Section title quadding
  ;; DESC
  ;; The quadding of section-level titles.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

(define %section-subtitle-quadding% 
  ;; REFENTRY section-subtitle-quadding
  ;; PURP Section subtitle quadding
  ;; DESC
  ;; The quadding of section-level subtitles.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  'start)

;; REFERENCE Bibliographies

(define biblio-citation-check
  ;; REFENTRY biblio-citation-check
  ;; PURP Check citations
  ;; DESC
  ;; If true, the content of CITATIONs will be checked against possible
  ;; biblioentries.  If the citation cannot be found, an error is issued
  ;; and the citation is generated.  If the citation is found, it is generated
  ;; with a cross reference to the appropriate biblioentry.
  ;;
  ;; A citation matches if the content of the citation element matches the
  ;; ID, XREFLABEL, or leading ABBREV of a biblioentry.
  ;;
  ;; This setting may have significant performance implications on large
  ;; documents, hence it is false by default.
  ;;
  ;; (This option can conveniently be set with '-V biblio-citation-check' 
  ;; on the Jade command line).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define biblio-filter-used
  ;; REFENTRY filter-used
  ;; PURP Suppress unreferenced bibliography entries
  ;; DESC
  ;; If true, bibliography entries which are not cited are suppressed.
  ;; A biblioentry is cited if an XREF or LINK matches its ID, or if
  ;; a CITE element matches its
  ;; ID, XREFLABEL, or leading ABBREV.
  ;;
  ;; A BIBLIOGRAPHY with no entries will still be output (making a whole
  ;; component conditional would be _A LOT_ of work and seems unnecessary),
  ;; but BIBLIDIVs with no entries will be suppressed.
  ;;
  ;; This setting may have significant performance implications,
  ;; hence it is false by default.
  ;;
  ;; (This option can conveniently be set with '-V biblio-filter-used' on the 
  ;; Jade command line).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define biblio-number
  ;; REFENTRY biblio-number
  ;; PURP Enumerate bibliography entries
  ;; DESC
  ;; If true, bibliography entries will be numbered.  If you cross-reference
  ;; bibliography entries, you should probably use biblio-number or
  ;; consistently use XREFLABEL or ABBREV.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define biblio-xref-title
  ;; REFENTRY biblio-xref-title
  ;; PURP Use the titles of bibliography entries in XREFs
  ;; DESC
  ;; If true, cross references to bibliography entries will use the
  ;; title of the entry as the cross reference text. Otherwise, either
  ;; the number (see 'biblio-number') or XREFLABEL/ABBREV will be used.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

;; REFERENCE OLinks

(define %olink-outline-ext%
  ;; REFENTRY olink-outline-ext
  ;; PURP Extension for olink outline file
  ;; DESC
  ;; The extension used to find the outline information file.  When searching
  ;; for outline information about a document, the extension is discarded
  ;; from the system ID of the file and '%olinke-outline-ext%' is appended.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".olink")

;; REFERENCE Footnotes

(define %footnote-ulinks%
  ;; REFENTRY footnote-ulinks
  ;; PURP Generate footnotes for ULinks?
  ;; DESC
  ;; If true, the URL of each ULink will appear as a footnote.
  ;; Processing ULinks this way may be very, very slow. It requires
  ;; walking over every descendant of every component in order to count
  ;; both ulinks and footnotes.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define bop-footnotes
  ;; REFENTRY bop-footnotes
  ;; PURP Make "bottom-of-page" footnotes?
  ;; DESC
  ;; If true, footnotes will be done at the bottom of the page instead 
  ;; of collected together as notes at the end of the section.
  ;; This variable is ignored if the print backend does not support
  ;; bottom-of-the-page footnotes. At present, only the TeX backend
  ;; supports them.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

;; REFERENCE Graphics

(define %graphic-default-extension% 
  ;; REFENTRY graphic-default-extension
  ;; PURP Default extension for graphic FILEREFs
  ;; DESC
  ;; The '%graphic-default-extension%' will be
  ;; added to the end of all 'fileref' filenames on
  ;; 'Graphic's if they do not end in one of the
  ;; '%graphic-extensions%'.  Set this to '#f'
  ;; to turn off this feature.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %graphic-extensions% 
  ;; REFENTRY graphic-extensions
  ;; PURP List of graphic filename extensions
  ;; DESC
  ;; The list of extensions which may appear on a 'fileref'
  ;; on a 'Graphic' which are indicative of graphic formats.
  ;;
  ;; Filenames that end in one of these extensions will not have
  ;; the '%graphic-default-extension%' added to them.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '("eps" "epsf" "gif" "tif" "tiff" "jpg" "jpeg" "png"))

(define image-library
  ;; REFENTRY image-library
  ;; PURP Load image library database for additional info about images?
  ;; DESC
  ;; If true, an image library database is loaded and extra information
  ;; about web graphics is retrieved from it.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define image-library-filename
  ;; REFENTRY image-library-filename
  ;; PURP Name of the image library database
  ;; DESC
  ;; If 'image-library' is true, then the database is loaded from
  ;; 'image-library-filename'.  It's a current limitation that only a
  ;; single database can be loaded.
  ;; 
  ;; The image library database is stored in a separate directory
  ;; because it must be parsed with the XML declaration.  The only
  ;; practical way to accomplish this with Jade, if you are processing a
  ;; document that uses another declaration, is by having a catalog
  ;; file in the directory that contains the image library that
  ;; specifies the SGMLDECL.  (So if it was in the same directory
  ;; as your document, your document would also be parsed with the
  ;; XML declaration, which may not be correct.)
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "imagelib/imagelib.xml")

;; REFERENCE Tables

(define ($table-element-list$) 
  ;; REFENTRY table-element-list
  ;; PURP List of table element names
  ;; DESC
  ;; The list of table elements in the DTD.  
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (list (normalize "table") (normalize "informaltable")))

(define %simplelist-column-width% 
  ;; REFENTRY simplelist-column-width
  ;; PURP Width of columns in tabular simple lists
  ;; DESC
  ;; If set to '#f', the table will span the entire
  ;; page width.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

;; REFERENCE VariableLists

(define %default-variablelist-termlength%
  ;; REFENTRY default-variablelist-termlength
  ;; PURP Default term length on variablelists
  ;; DESC
  ;; When formatting a 'VariableList', this value is used as the
  ;; default term length, if no 'TermLength' is specified.
  ;;
  ;; If all of the terms in a list shorter than the term length,
  ;; the stylesheet may format them "side-by-side" in a table if
  ;; 'may-format-variablelist-as-table' is '#t'.
  ;; /DESC
  ;; /REFENTRY
  20)

(define %may-format-variablelist-as-table%
  ;; REFENTRY may-format-variablelist-as-table
  ;; PURP Format VariableLists as tables?
  ;; DESC
  ;; If '%may-format-variablelist-as-table%' is '#t', a
  ;; 'VariableList' will be formatted as a table, if *all of*
  ;; the terms are shorter than the specified 'TermLength'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %always-format-variablelist-as-table%
  ;; REFENTRY always-format-variablelist-as-table
  ;; PURP Always format VariableLists as tables?
  ;; DESC
  ;; When a 'VariableList' is formatted, if any of the
  ;; terms in the list are too long, the whole list is formatted as a
  ;; list.
  ;;
  ;; If '%always-format-variablelist-as-table%' is
  ;; '#t', the 'VariableList' will be
  ;; formatted as a table, even if some terms are too long.  The terms that
  ;; are too long will format span above their associated description.
  ;; /DESC
  ;; /REFENTRY
  #f)

;; REFERENCE Vertical Spacing

(define %line-spacing-factor% 
  ;; REFENTRY line-spacing-factor
  ;; PURP Factor used to calculate leading
  ;; DESC
  ;; The leading is calculated by multiplying the current font size by the 
  ;; '%line-spacing-factor%'. For example, if the font size is 10pt and
  ;; the '%line-spacing-factor%' is 1.1, then the text will be
  ;; printed "10-on-11".
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  1.3)

(define %head-before-factor% 
  ;; REFENTRY head-before-factor
  ;; PURP Factor used to calculate space above a title
  ;; DESC
  ;; The space before a title is calculated by multiplying the font size
  ;; used in the title by the '%head-before-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.75)

(define %head-after-factor% 
  ;; REFENTRY head-after-factor
  ;; PURP Factor used to calculate space below a title
  ;; DESC
  ;; The space after a title is calculated by multiplying the font size used
  ;; in the title by the '%head-after-factor%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0.5)

(define %body-start-indent% 
  ;; REFENTRY body-start-indent
  ;; PURP Default indent of body text
  ;; DESC
  ;; The default indent of body text. Some elements may have more or less
  ;; indentation.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  4pi)

(define %para-sep% 
  ;; REFENTRY para-sep
  ;; PURP Distance between paragraphs
  ;; DESC
  ;; The '%para-sep%' is the distance between the last line
  ;; of one paragraph and the first line of the next.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (/ %bf-size% 2.0))

(define %block-sep% 
  ;; REFENTRY block-sep
  ;; PURP Distance between block-elements
  ;; DESC
  ;; The '%block-sep%' is the vertical distance between
  ;; block elements (figures, tables, etc.)
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  (* %para-sep% 2.0))

;; REFERENCE Indents

(define %para-indent%
  ;; REFENTRY para-indent
  ;; PURP First line start-indent for paragraphs (other than the first)
  ;; DESC
  ;; The '%para-indent%' is the amount of extra indentation that the
  ;; first line of a paragraph should receive.  This parameter applies
  ;; only to the second and subsequent paragraphs in a section.  See
  ;; '%para-indent-firstpara%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0pt)

(define %para-indent-firstpara%
  ;; REFENTRY para-indent-firstpara
  ;; PURP First line start-indent for the first paragraph
  ;; DESC
  ;; The '%para-indent-firstpara%' is the amount of extra indentation
  ;; that the first line of the first paragraph of a section should receive.
  ;; This parameter is frequently '0pt' even when '%para-indent%' is 
  ;; not.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0pt)

(define %block-start-indent% 
  ;; REFENTRY block-start-indent
  ;; PURP Extra start-indent for block-elements
  ;; DESC
  ;; Block elements (tables, figures, verbatim environments, etc.) will
  ;; be indented by the specified amount.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  0pt)

;; REFERENCE Object Rules

(define %example-rules%
  ;; REFENTRY example-rules
  ;; PURP Specify rules before and after an Example
  ;; DESC
  ;; If '#t', rules will be drawn before and after each
  ;; 'Example'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %figure-rules%
  ;; REFENTRY figure-rules
  ;; PURP Specify rules before and after an Figure
  ;; DESC
  ;; If '#t', rules will be drawn before and after each
  ;; 'Figure'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %table-rules%
  ;; REFENTRY table-rules
  ;; PURP Specify rules before and after an Table
  ;; DESC
  ;; If '#t', rules will be drawn before and after each
  ;; 'Table'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %equation-rules%
  ;; REFENTRY equation-rules
  ;; PURP Specify rules before and after an Equation
  ;; DESC
  ;; If '#t', rules will be drawn before and after each
  ;; 'Equation'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %informalexample-rules%
  ;; REFENTRY informalexample-rules
  ;; PURP Specify rules before and after an InformalExample
  ;; DESC
  ;; If '#t', rules will be drawn before and after each
  ;; 'InformalExample'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %informalfigure-rules%
  ;; REFENTRY informalfigure-rules
  ;; PURP Specify rules before and after an InformalFigure
  ;; DESC
  ;; If '#t', rules will be drawn before and after each
  ;; 'InformalFigure'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %informaltable-rules%
  ;; REFENTRY informaltable-rules
  ;; PURP Specify rules before and after an InformalTable
  ;; DESC
  ;; If '#t', rules will be drawn before and after each
  ;; 'InformalTable'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %informalequation-rules%
  ;; REFENTRY informalequation-rules
  ;; PURP Specify rules before and after an InformalEquation
  ;; DESC
  ;; If '#t', rules will be drawn before and after each
  ;; 'InformalEquation'.
  ;; /DESC
  ;; /REFENTRY
  #f)

(define %object-rule-thickness%
  ;; REFENTRY object-rule-thickness
  ;; PURP Width of rules around formal and informal objects
  ;; DESC
  ;; Specifies the width of the rules drawn before and after an object.
  ;; This only applies if the appropriate
  ;; '%*-rules%' variable
  ;; is '#t'.
  ;; /DESC
  ;; /REFENTRY
  2pt)

;; REFERENCE Miscellaneous

(define ($object-titles-after$)
  ;; REFENTRY object-titles-after
  ;; PURP List of objects who's titles go after the object
  ;; DESC
  ;; Titles of formal objects (Figures, Equations, Tables, etc.)
  ;; in this list will be placed below the object instead of above it.
  ;;
  ;; This is a list of element names, for example:
  ;; '(list (normalize "figure") (normalize "table"))'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '())

(define formal-object-float
  ;; REFENTRY formal-object-float
  ;; PURP Do formal objects float?
  ;; DESC
  ;; If '#t', formal objects will float if floating is supported by the
  ;; backend. At present, only the TeX backend supports floats.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %default-title-end-punct% 
  ;; REFENTRY default-title-end-punct
  ;; PURP Default punctuation at the end of a run-in head.
  ;; DESC
  ;; The punctuation used at the end of a run-in head (e.g. on FORMALPARA).
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".")

(define %content-title-end-punct% 
  ;; REFENTRY content-title-end-punct
  ;; PURP List of punctuation chars at the end of a run-in head
  ;; DESC
  ;; If a run-in head ends in any of these characters, the
  ;; '%default-title-end-punct%' is not used.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '(#\. #\! #\? #\:))

(define %honorific-punctuation%
  ;; REFENTRY honorific-punctuation
  ;; PURP Punctuation to follow honorifics in names
  ;; DESC
  ;; The honorific punctuation is placed after the honorific in
  ;; a name. 
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  ".")

(define %default-simplesect-level% 
  ;; REFENTRY default-simplesect-level
  ;; PURP Default section level for 'SimpleSect's.
  ;; DESC
  ;; If 'SimpleSect's appear inside other section-level
  ;; elements, they are rendered at the appropriate section level, but if they
  ;; appear in a component-level element, they are rendered at 
  ;; '%default-simplesect-level%'.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  4)

(define %show-ulinks%
  ;; REFENTRY show-ulinks
  ;; PURP Display URLs after ULinks?
  ;; DESC
  ;; If true, the URL of each ULink will appear in parenthesis after
  ;; the text of the link.  If the text of the link and the URL are
  ;; identical, the parenthetical URL is suppressed.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define %show-comments%
  ;; REFENTRY show-comments
  ;; PURP Display Comment elements?
  ;; DESC
  ;; If true, comments will be displayed, otherwise they are suppressed.
  ;; Comments here refers to the 'Comment' element, which will be renamed
  ;; 'Remark' in DocBook V4.0, not SGML/XML comments which are unavailable.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #t)

(define firstterm-bold
  ;; REFENTRY firstterm-bold
  ;; PURP Make FIRSTTERM elements bold?
  ;; DESC
  ;; If '#t', FIRSTTERMs will be bold, to distinguish them from
  ;; simple GLOSSTERMs.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %min-leading%
  ;; REFENTRY min-leading
  ;; PURP Minumum leading between lines
  ;; DESC
  ;; The '%min-leading%' parameter specifies the smallest amount of leading
  ;; to allow between lines.  The default value, '#f', has the side-effect
  ;; that leading cannot change.  This means that graphics that appear in
  ;; a paragraph are truncated if they are taller than the current leading.
  ;; By setting this parameter to some small value, we get stretchable
  ;; space between lines.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  2pt)

(define %hyphenation%
  ;; REFENTRY hyphenation
  ;; PURP Allow automatic hyphenation?
  ;; DESC
  ;; The '%hyphenation%' parameter indicates whether or
  ;; not the backend should allow automatic hyphention of text, for example
  ;; in paragraphs. The default value, '#f', indicates that
  ;; it should not.
  ;; /DESC
  ;; /REFENTRY
  #f)

(declare-initial-value writing-mode 	%writing-mode%)

(declare-initial-value input-whitespace-treatment 'collapse)

(declare-initial-value left-margin 	%left-margin%)
(declare-initial-value right-margin 	%right-margin%)

(declare-initial-value page-width	%page-width%)
(declare-initial-value page-height	%page-height%)

(declare-initial-value min-leading %min-leading%)
(declare-initial-value top-margin	%top-margin%)
(declare-initial-value bottom-margin	%bottom-margin%)
(declare-initial-value header-margin	%header-margin%)
(declare-initial-value footer-margin	%footer-margin%)

</style-specification-body>
</style-specification>
</style-sheet>
