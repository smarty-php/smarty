;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ................... INDEX TERMS (EMBEDDED MARKERS) ...................

(element indexterm 
  (if html-index
      (let* ((id (if (attribute-string (normalize "id"))
		     (attribute-string (normalize "id"))
		     (generate-anchor))))
	(make element gi: "A"
	      attributes: (list (list "NAME" id))
	      (empty-sosofo)))
      (empty-sosofo)))

(element primary (empty-sosofo))
(element secondary (empty-sosofo))
(element tertiary (empty-sosofo))
(element see (empty-sosofo))
(element seealso (empty-sosofo))

;; =========================== INDEX ELEMENTS ===========================

(element (setindex title) (empty-sosofo))
(element setindex
  (let ((preamble (node-list-filter-by-not-gi 
		   (children (current-node))
		   (list (normalize "indexentry"))))
	(entries  (node-list-filter-by-gi
		   (children (current-node))
		   (list (normalize "indexentry")))))
    (html-document 
     (with-mode head-title-mode 
       (literal (element-title-string (current-node))))
     (make element gi: "DIV"
	   attributes: (list (list "CLASS" (gi)))
	   ($component-separator$)
	   ($component-title$)
	   (process-node-list preamble)
	   (if (node-list-empty? entries)
	       (empty-sosofo)
	       (make element gi: "DL"
		     (process-node-list entries)))))))

(element (index title) (empty-sosofo))
(element index 
  (let ((preamble (node-list-filter-by-not-gi 
		   (children (current-node))
		   (list (normalize "indexentry"))))
	(entries  (node-list-filter-by-gi
		   (children (current-node))
		   (list (normalize "indexentry")))))
    (html-document 
     (with-mode head-title-mode 
       (literal (element-title-string (current-node))))
     (make element gi: "DIV"
	   attributes: (list (list "CLASS" (gi)))
	   ($component-separator$)
	   ($component-title$)
	   (process-node-list preamble)
	   (if (node-list-empty? entries)
	       (empty-sosofo)
	       (make element gi: "DL"
		     (process-node-list entries)))))))


(element (indexdiv title) (empty-sosofo))
(element indexdiv
  (let ((preamble (node-list-filter-by-not-gi 
		   (children (current-node))
		   (list (normalize "indexentry"))))
	(entries  (node-list-filter-by-gi
		   (children (current-node))
		   (list (normalize "indexentry")))))
    (html-document
     (with-mode head-title-mode
       (literal (element-title-string (current-node))))
     (make element gi: "DIV"
	   attributes: (list (list "CLASS" (gi)))
	   ($section-separator$)
	   ($section-title$)
	   (process-node-list preamble)
	   (if (node-list-empty? entries)
	       (empty-sosofo)
	       (make element gi: "DL"
		     (process-node-list entries)))))))

(define (break-node-list nodes breakatgi)
  ;; Given a _node_ list "PRIM SEC TERT SEC SEC TERT PRIM SEC PRIM PRIM"
  ;; and the breakatgi of "PRIM", returns the _list_ of _node_ lists:
  ;; '("PRIM SEC TERT SEC SEC TERT" "PRIM SEC" "PRIM" "PRIM")
  (let loop ((nl nodes) (result '()) (curlist (empty-node-list)))
    (if (node-list-empty? nl)
	(if (node-list-empty? curlist)
	    result
	    (append result (list curlist)))
	(if (equal? (gi (node-list-first nl)) breakatgi)
	    (loop (node-list-rest nl)
		  (if (node-list-empty? curlist)
		      result
		      (append result (list curlist)))
		  (node-list-first nl))
	    (loop (node-list-rest nl)
		  result
		  (node-list curlist (node-list-first nl)))))))

(define (process-primary primnode secnl)
  (let ((see?     (equal? (gi (node-list-first secnl)) 
			  (normalize "seeie")))
	(seealso? (equal? (gi (node-list-first secnl))
			  (normalize "seealsoie")))
	(second   (break-node-list secnl (normalize "secondaryie"))))
    (if (or see? seealso?)
	(process-terminal primnode secnl #t)
	(make sequence
	  (process-nonterminal primnode)
	  (if (node-list-empty? secnl)
	      (empty-sosofo)
	      (make element gi: "DD"
		    (make element gi: "DL"
			  (let sloop ((secs second))
			    (if (null? secs)
				(empty-sosofo)
				(make sequence
				  (let* ((nodes (car secs))
					 (sec   (node-list-first nodes))
					 (terts (node-list-rest nodes)))
				    (process-secondary sec terts))
				  (sloop (cdr secs))))))))))))

(define (process-secondary secnode tertnl)
  (let ((see?     (equal? (gi (node-list-first tertnl))
			  (normalize "seeie")))
	(seealso? (equal? (gi (node-list-first tertnl))
			  (normalize "seealsoie")))
	(tert (break-node-list tertnl (normalize "tertiaryie"))))
    (if (or see? seealso?)
	(process-terminal secnode tertnl)
	(make sequence
	  (process-nonterminal secnode)
	  (make element gi: "DD"
		(make element gi: "DL"
		      (let tloop ((terts tert))
			(if (null? terts)
			    (empty-sosofo)
			    (make sequence
			      (let* ((nodes (car terts))
				     (tert  (node-list-first nodes))
				     (sees  (node-list-rest nodes)))
				(process-tertiary tert sees))
			      (tloop (cdr terts)))))))))))

(define (process-tertiary tertnode seenl)
  (process-terminal tertnode seenl))

(define (process-terminal node seenl #!optional (output-id #f))
  (let ((id (attribute-string (normalize "id") (parent node))))
    (make sequence
      (make element gi: "DT"
	    (if id
		(make element gi: "A"
		      attributes: (list (list "NAME" id))
		      (empty-sosofo))
		(empty-sosofo))
	    (process-node-list node))
      (if (node-list-empty? seenl)
	  (empty-sosofo)
	  (make element gi: "DD"
		(make element gi: "DL"
		      (let loop ((nl seenl))
			(if (node-list-empty? nl)
			    (empty-sosofo)
			    (make sequence
			      (make element gi: "DT"
				    (process-node-list 
				     (node-list-first nl)))
			      (loop (node-list-rest nl)))))))))))

(define (process-nonterminal node)
  (make element gi: "DT"
	(process-node-list node)))

(element indexentry
  (let* ((primary   (break-node-list (children (current-node))
				     (normalize "primaryie"))))
    (make sequence
      (let ploop ((prims primary))
	(if (null? prims)
	    (empty-sosofo)
	    (make sequence
	      (let* ((nodes (car prims))
		     (prim  (node-list-first nodes))
		     (secs  (node-list-rest nodes)))
		(process-primary prim secs))
	      (ploop (cdr prims))))))))

(element primaryie (process-children))
(element secondaryie (process-children))
(element tertiaryie (process-children))

(define (indexentry-link nd)
  (let* ((preferred (not (node-list-empty?
			  (select-elements (children (current-node))
					   (normalize "emphasis"))))))
    (make element gi: "A"
	  attributes: (list (list "HREF" 
				  (attribute-string (normalize "url"))))
	  (process-children))))

(element (primaryie ulink)
  (indexentry-link (current-node)))

(element (secondaryie ulink)
  (indexentry-link (current-node)))

(element (tertiaryie ulink)
  (indexentry-link (current-node)))

(element seeie 
  (let ((linkend (attribute-string (normalize "linkend"))))
      (if linkend
	  (make element gi: "A"
		attributes: (list (list "HREF" 
					(href-to (element-with-id linkend))))
		(literal (gentext-element-name (current-node)))
		(literal (gentext-label-title-sep (current-node)))
		(process-children))
	  (make sequence
	    (literal (gentext-element-name (current-node)))
	    (literal (gentext-label-title-sep (current-node)))
	    (process-children)))))

(element seealsoie
  (let* ((alinkends (attribute-string (normalize "linkends")))
	 (linkends  (if alinkends
			(split alinkends)
			'()))
	 (linkend   (if alinkends
			(car linkends)
			#f)))
    (if linkend
	(make element gi: "A"
	      attributes: (list (list "HREF" 
				      (href-to (element-with-id linkend))))
	      (literal (gentext-element-name (current-node)))
	      (literal (gentext-label-title-sep (current-node)))
	      (process-children))
	(make sequence
	  (literal (gentext-element-name (current-node)))
	  (literal (gentext-label-title-sep (current-node)))
	  (process-children)))))

;; =====================HTML INDEX PROCESSING ==============================

(define (htmlnewline)
  (make formatting-instruction data: "&#13;"))

(define (htmlindexattr attr)
  (if (attribute-string (normalize attr))
      (make sequence
	(make formatting-instruction data: attr)
	(make formatting-instruction data: " ")
	(make formatting-instruction data: (attribute-string 
					    (normalize attr)))
	(htmlnewline))
      (empty-sosofo)))

(define (htmlindexterm)
  (let* ((attr    (gi (current-node)))
	 (content (data (current-node)))
	 (string  (string-replace content "&#13;" " "))
	 (sortas  (attribute-string (normalize "sortas"))))
    (make sequence
      (make formatting-instruction data: attr)
      (if sortas
	  (make sequence
	    (make formatting-instruction data: "[")
	    (make formatting-instruction data: sortas)
	    (make formatting-instruction data: "]"))
	  (empty-sosofo))
      (make formatting-instruction data: " ")
      (make formatting-instruction data: string)
      (htmlnewline))))

(define (htmlindexzone zone)
  (let loop ((idlist (split zone)))
    (if (null? idlist)
	(empty-sosofo)
	(make sequence
	  (htmlindexzone1 (car idlist))
	  (loop (cdr idlist))))))

(define (htmlindexzone1 id)
  (let* ((target (ancestor-member (element-with-id id)
				  (append (book-element-list)
					  (division-element-list)
					  (component-element-list)
					  (section-element-list))))
	 (title  (string-replace (element-title-string target) "&#13;" " ")))
    (make sequence
      (make formatting-instruction data: "ZONE ")
      (make formatting-instruction data: (href-to target))
      (htmlnewline)

      (make formatting-instruction data: "TITLE ")
      (make formatting-instruction data: title)
      (htmlnewline))))

(mode htmlindex
  ;; this mode is really just a hack to get at the root element
  (root (process-children))

  (default 
    (if (node-list=? (current-node) (sgml-root-element))
	(make entity
	  system-id: (html-entity-file html-index-filename)
	  (process-node-list (select-elements 
			      (descendants (current-node))
			      (normalize "indexterm"))))
	(empty-sosofo)))

  (element indexterm
    (let* ((target (ancestor-member (current-node)
				    (append (book-element-list)
					    (division-element-list)
					    (component-element-list)
					    (section-element-list))))
	   (title  (string-replace (element-title-string target) "&#13;" " ")))
      (make sequence
	(make formatting-instruction data: "INDEXTERM ")
	(make formatting-instruction data: (href-to target))
	(htmlnewline)

	(make formatting-instruction data: "INDEXPOINT ")
	(make formatting-instruction data: (href-to (current-node)))
	(htmlnewline)

	(make formatting-instruction data: "TITLE ")
	(make formatting-instruction data: title)
	(htmlnewline)

	(htmlindexattr "scope")
	(htmlindexattr "significance")
	(htmlindexattr "class")
	(htmlindexattr "id")
	(htmlindexattr "startref")
	
	(if (attribute-string (normalize "zone"))
	    (htmlindexzone (attribute-string (normalize "zone")))
	    (empty-sosofo))

	(process-children)

	(make formatting-instruction data: "/INDEXTERM")
	(htmlnewline))))
		    
  (element primary
    (htmlindexterm))

  (element secondary
    (htmlindexterm))

  (element tertiary
    (htmlindexterm))

  (element see
    (htmlindexterm))

  (element seealso
    (htmlindexterm))
)
