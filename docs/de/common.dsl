;; -*- Scheme -*-
;;
;; common.dsl,v 1.1.1.1 2003/01/09 09:44:46 dexxter Exp
;;
;; This file contains stylesheet customization common to the HTML
;; and print versions.
;;

;; Stylesheets Localization
(define %default-language% "en")

(define %use-id-as-filename% #t)
(define %gentext-nav-tblwidth% "100%")
(define %refentry-function% #t)
(define %refentry-generate-name% #f)
(define %funcsynopsis-style% 'ansi)
(define ($legalnotice-link-file$ legalnotice)
  (string-append "copyright" %html-ext%))
(define %generate-legalnotice-link% #t)
(define %footnotes-at-end% #t)
(define %force-chapter-toc% #t)
(define newline "\U-000D")
(define %number-programlisting-lines% #f)
(define %linenumber-mod% 1)
(define %shade-verbatim% #t)

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
  (list (normalize "table")))

(define (php-code code)
  (make processing-instruction
    data: (string-append "php " code "?")))
