;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ................... INDEX TERMS (EMBEDDED MARKERS) ...................

(element indexterm
  ;; This is different than (empty-sosofo) alone because the backend
  ;; will hang an anchor off the empty sequence.  This allows the index
  ;; to point to the indexterm (but only if the indexterm has an ID).
  (make sequence (empty-sosofo)))

(element primary (empty-sosofo))
(element secondary (empty-sosofo))
(element tertiary (empty-sosofo))
(element see (empty-sosofo))
(element seealso (empty-sosofo))

;; =========================== INDEX ELEMENTS ===========================

(element setindex ($component$))
(element (setindex title) (empty-sosofo))

(element index
  (make simple-page-sequence
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
    page-n-columns: 2
    (make sequence
      ($component-title$)
      (process-children))
    (make-endnotes)))

;; this is a special case. this prevents the index from causing an error but
;; will make the index a single column. c'est la vie.
(element (article index) ($section$))

(element (index title) (empty-sosofo))

(element indexdiv ($section$))
(element (indexdiv title) (empty-sosofo))

(element indexentry (process-children))

(element primaryie
  (make paragraph
    font-size: (* (inherited-font-size) %smaller-size-factor%)
    (process-children)))

(element secondaryie
  (make paragraph
    font-size: (* (inherited-font-size) %smaller-size-factor%)
    start-indent: (+ (inherited-start-indent) 1em)
    (process-children)))

(element tertiaryie
  (make paragraph
    font-size: (* (inherited-font-size) %smaller-size-factor%)
    start-indent: (+ (inherited-start-indent) 2em)
    (process-children)))

(define (find-indexterm id)
  ;; If you have a lot of indexterms that don't have IDs, this could 
  ;; be incredibly slow.  So don't do that.
  (let* ((idtarget (element-with-id id)))
    (if (node-list-empty? idtarget)
	(let loop ((idnodes (select-elements (descendants (sgml-root-element))
					     (normalize "indexterm"))))
	  (if (node-list-empty? idnodes)
	      (empty-node-list)
	      (if (equal? id (string-append "AEN" 
					    (number->string 
					     (all-element-number 
					      (node-list-first idnodes)))))
		  (node-list-first idnodes)
		  (loop (node-list-rest idnodes)))))
	idtarget)))

(define (indexentry-link nd)
  (let* ((id        (attribute-string (normalize "role") nd))
	 (target    (find-indexterm id))
	 (preferred (not (node-list-empty?
			  (select-elements (children (current-node))
					   (normalize "emphasis")))))
	 (sosofo    (if (node-list-empty? target) 
			(literal "?")
			(make link
			  destination: (node-list-address target)
			  (with-mode toc-page-number-mode
			    (process-node-list target))))))
    (if preferred
	(make sequence
	  font-weight: 'bold
	  sosofo)
	sosofo)))

(element (primaryie ulink)
  (indexentry-link (current-node)))

(element (secondaryie ulink)
  (indexentry-link (current-node)))

(element (tertiaryie ulink)
  (indexentry-link (current-node)))

(element seeie
  (let ((indent (cond ((node-list-empty? 
			(select-elements
			 (children (parent (current-node)))
			 (normalize "secondaryie")))
		       1em)
		      ((node-list-empty? 
			(select-elements
			 (children (parent (current-node)))
			 (normalize "tertiaryie")))
		       2em)
		      (else 3em))))
    (make paragraph
      font-size: (* (inherited-font-size) %smaller-size-factor%)
      start-indent: (+ (inherited-start-indent) indent)
      (literal "(" (gentext-index-see) " ")
      (process-children)
      (literal ")"))))

(element seealsoie
  (let ((indent (cond ((node-list-empty? 
			(select-elements
			 (children (parent (current-node)))
			 (normalize "secondaryie")))
		       1em)
		      ((node-list-empty? 
			(select-elements
			 (children (parent (current-node)))
			 (normalize "tertiaryie")))
		       2em)
		      (else 3em))))
    (make paragraph
      font-size: (* (inherited-font-size) %smaller-size-factor%)
      start-indent: (+ (inherited-start-indent) indent)
      (literal "(" (gentext-index-seealso) " ")
      (process-children)
      (literal ")"))))
