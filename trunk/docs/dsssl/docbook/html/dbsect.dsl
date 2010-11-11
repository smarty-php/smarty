;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================== SECTIONS ==============================

(define (SECTLEVEL #!optional (sect (current-node)))
  (section-level-by-node (not nochunks) sect))

;; BRIDGEHEAD isn't a proper section, but appears to be a section title
(element bridgehead
  (let* ((renderas (attribute-string "renderas"))
	 ;; the apparent section level
	 (hlevel
	  ;; if not real section level, then get the apparent level
	  ;; from "renderas"
	  (if renderas
	      (section-level-by-gi (not nochunks) (normalize renderas))
	      ;; else use the real level
	      (SECTLEVEL)))
	 (helem
	  (string-append "H" (number->string hlevel))))
    (make element gi: helem
	  attributes: '(("CLASS" "BRIDGEHEAD"))
	  (make element gi: "A"
		attributes: (list (list "NAME" (element-id)))
		(empty-sosofo))
	  (process-children))))

(define ($section-separator$) 
  (let* (;; There are several situations in which we don't want a
	 ;; separator here:
	 ;; 1. This document is being chunked:
	 (chunks (not nochunks))
	 ;; 2. This node is the root element of the document:
	 (isroot (node-list=? (current-node) (sgml-root-element)))
	 ;; 3. This node is the first section in the root element
	 ;;    and no other content (except the *info elements and
	 ;;    the title) precedes it.  This means that the
	 ;;    titlepage-separator was the last thing we put out.
	 ;;    No one expects two separators in a row, or the Spanish
	 ;;    inquisition.
	 (s1ofrt  (node-list=? (parent (current-node)) (sgml-root-element)))
	 (precnd  (ipreced (current-node)))
	 (infond  (info-element (parent (current-node))))
	 (isfirst (or (equal? (gi precnd) (normalize "title"))
		      (node-list=? precnd infond))))
    (if (or chunks isroot isfirst)
	(empty-sosofo)
	(make empty-element gi: "HR"))))

(define ($section$)
  (html-document (with-mode head-title-mode 
		   (literal (element-title-string (current-node))))
		 ($section-body$)))

(define ($section-body$)
  (make element gi: "DIV"
	attributes: (list (list "CLASS" (gi)))
	($section-separator$)
	($section-title$)

	(if (not (node-list-empty? (select-elements (children (current-node))
						    (normalize "refentry"))))
	    (build-toc (current-node) 1)
	    (empty-sosofo))

	(process-children)))

(define ($section-title$)
  (let* ((sect (current-node))
	 (info (info-element))
	 (subtitles (select-elements (children info) (normalize "subtitle")))
	 (renderas (inherited-attribute-string (normalize "renderas") sect))
	 ;; the apparent section level
	 (hlevel
	  ;; if not real section level, then get the apparent level
	  ;; from "renderas"
	  (if renderas
	      (section-level-by-gi (not nochunks) (normalize renderas))
	      ;; else use the real level
	      (SECTLEVEL)))
	 (h1elem
	  (string-append "H" (number->string hlevel)))
	 (h2elem
	  (string-append "H" (number->string (+ hlevel 1))))
	 (name (element-id))
	 (isep (gentext-intra-label-sep (gi sect)))
	 (nsep (gentext-label-title-sep (gi sect))))
    (make sequence
      (make element gi: h1elem
	    attributes: (list (list "CLASS" (gi sect)))
	    (make sequence
	      (make element gi: "A"
		    attributes: (list (list "NAME" name))
		    (empty-sosofo))
	      (if (string=? (element-label (current-node)) "")
		  (empty-sosofo)
		  (literal (element-label (current-node)) nsep))
	      (element-title-sosofo sect)))
      (if (node-list-empty? subtitles) 
	  (empty-sosofo)
	  (with-mode subtitle-mode
	    (make element gi: h2elem
		  (process-node-list subtitles))))
      ($proc-section-info$ info))))

(define ($proc-section-info$ info)
  (cond ((equal? (gi) (normalize "sect1"))
	 ($sect1-info$ info))
	((equal? (gi) (normalize "sect2"))
	 ($sect2-info$ info))
	((equal? (gi) (normalize "sect3"))
	 ($sect3-info$ info))
	((equal? (gi) (normalize "sect4"))
	 ($sect4-info$ info))
	((equal? (gi) (normalize "sect5"))
	 ($sect5-info$ info))
	((equal? (gi) (normalize "section"))
	 ($section-info$ info))
	((equal? (gi) (normalize "refsect1"))
	 ($refsect1-info$ info))
	((equal? (gi) (normalize "refsect2"))
	 ($refsect2-info$ info))
	((equal? (gi) (normalize "refsect3"))
	 ($refsect3-info$ info))
	(else (empty-sosofo))))

(define ($sect1-info$ info) (empty-sosofo))
(define ($sect2-info$ info) (empty-sosofo))
(define ($sect3-info$ info) (empty-sosofo))
(define ($sect4-info$ info) (empty-sosofo))
(define ($sect5-info$ info) (empty-sosofo))
(define ($section-info$ info) (empty-sosofo))
(define ($refsect1-info$ info) (empty-sosofo))
(define ($refsect2-info$ info) (empty-sosofo))
(define ($refsect3-info$ info) (empty-sosofo))

(element sect1 ($section$))
(element (sect1 title) (empty-sosofo))

(element sect2 ($section$))
(element (sect2 title) (empty-sosofo))

(element sect3 ($section$))
(element (sect3 title) (empty-sosofo))

(element sect4 ($section$))
(element (sect4 title) (empty-sosofo))

(element sect5 ($section$))
(element (sect5 title) (empty-sosofo))

(element simplesect ($section$))
(element (simplesect title) (empty-sosofo))

