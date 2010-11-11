;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================== TABLE OF CONTENTS =========================

;; Returns the depth of auto TOC that should be made at the nd-level
(define (toc-depth nd)
  (if (string=? (gi nd) (normalize "book"))
      3
      1))

(define (toc-entry tocentry)
  (make element gi: "DT"
	(make sequence
	  (if (equal? (element-label tocentry) "")
	      (empty-sosofo)
	      (make sequence
		(literal (element-label tocentry))
		(literal (gentext-label-title-sep 
			  (gi tocentry)))))

	  ;; If the tocentry isn't in its own
	  ;; chunk, don't make a link...
	  (if (and #f (not (chunk? tocentry)))
	      (element-title-sosofo tocentry)
	      (make element gi: "A"
		    attributes: (list
				 (list "HREF"
				       (href-to tocentry)))
		    (element-title-sosofo tocentry)))

	  ;; Maybe annotate...
	  (if (and %annotate-toc% 
		   (equal? (gi tocentry) (normalize "refentry")))
	      (make sequence
		(dingbat-sosofo "nbsp");
		(dingbat-sosofo "em-dash");
		(dingbat-sosofo "nbsp");
		(toc-annotation tocentry))
	      (empty-sosofo)))))

(define (toc-annotation tocentry)
  ;; only handles refentry at the moment
  (let* ((refnamediv (select-elements (children tocentry) 
				      (normalize "refnamediv")))
	 (refpurpose (select-elements (children refnamediv)
				      (normalize "refpurpose"))))
    (process-node-list (children refpurpose))))

(define (build-toc nd depth #!optional (chapter-toc? #f) (first? #t))
  (let ((toclist (toc-list-filter 
		  (node-list-filter-by-gi (children nd)
					  (append (division-element-list)
						  (component-element-list)
						  (section-element-list)))))
	(wrappergi (if first? "DIV" "DD"))
	(wrapperattr (if first? '(("CLASS" "TOC")) '())))
    (if (or (<= depth 0) 
	    (node-list-empty? toclist)
	    (and chapter-toc?
		 (not %force-chapter-toc%)
		 (<= (node-list-length toclist) 1)))
	(empty-sosofo)
	(make element gi: wrappergi
	      attributes: wrapperattr
	      (make element gi: "DL"
		    (if first?
			(make element gi: "DT"
			      (make element gi: "B"
				    (literal (gentext-element-name (normalize "toc")))))
			(empty-sosofo))
		    (let loop ((nl toclist))
		      (if (node-list-empty? nl)
			  (empty-sosofo)
			  (sosofo-append
			    (toc-entry (node-list-first nl))
			    (build-toc (node-list-first nl) 
				       (- depth 1) chapter-toc? #f)
			    (loop (node-list-rest nl))))))))))

;; Print the LOT entry
(define (lot-entry tocentry)
  (make element gi: "DT"
	(make sequence
	  (if (equal? (element-label tocentry) "")
	      (empty-sosofo)
	      (make sequence
		(literal (element-label tocentry))
		(literal (gentext-label-title-sep 
			  (gi tocentry)))))

	  ;; If the tocentry isn't in its own
	  ;; chunk, don't make a link...
	  (if (and #f (not (chunk? tocentry)))
	      (element-title-sosofo tocentry)
	      (make element gi: "A"
		    attributes: (list
				 (list "HREF"
				       (href-to tocentry)))
		    (element-title-sosofo tocentry))))))

;; Build a LOT starting at nd for all the lotgi's it contains.
;; The optional arguments are used on recursive calls to build-toc
;; and shouldn't be set by the initial caller...
;;

(define (build-lot nd lotgi)
  (let* ((lotlist (select-elements (descendants nd)
				   (normalize lotgi))))
    (if (node-list-empty? lotlist)
	(empty-sosofo)
	(make element gi: "DIV"
	      attributes: '(("CLASS" "LOT"))
	      (make element gi: "DL"
		    attributes: '(("CLASS" "LOT"))
		    (make element gi: "DT"
			  (make element gi: "B"
				(literal ($lot-title$ 
					  (gi (node-list-first lotlist))))))
		    (let loop ((lote lotlist))
		      (if (node-list-empty? lote)
			  (empty-sosofo)
			  (make sequence
			    (lot-entry (node-list-first lote))
			    (loop (node-list-rest lote))))))))))
