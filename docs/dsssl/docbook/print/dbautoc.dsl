;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ========================== TABLE OF CONTENTS =========================

(define %toc-indent% 2pi)
(define %toc-spacing-factor% 0.4)

;; Returns the depth of auto TOC that should be made at the nd-level
(define (toc-depth nd)
  (if (string=? (gi nd) (normalize "book"))
      7
      1))

(define (format-page-number)
  (current-node-page-number-sosofo))

;; Prints the TOC title if first? is true, otherwise does nothing
(define (toc-title first?)
  (let ((hsize (if (or (equal? (gi (current-node)) (normalize "article"))
		       (equal? (gi (current-node)) (normalize "part")))
		   (HSIZE 3)
		   (HSIZE 4))))
    (if first?
	(make paragraph
	  font-family-name: %title-font-family%
	  font-weight: 'bold
	  font-size: hsize
	  line-spacing: (* hsize %line-spacing-factor%)
	  space-before: (* hsize %head-before-factor%)
	  space-after: (* hsize %head-after-factor%)
	  start-indent: 0pt
	  first-line-start-indent: 0pt
	  quadding: %component-title-quadding%
	  heading-level: (if %generate-heading-level% 1 0)
	  keep-with-next?: #t
	  (literal (gentext-element-name (normalize "toc"))))
	(empty-sosofo))))

;; Prints the TOC title if first? is true, otherwise does nothing
(define (lot-title first? lotgi)
  (if first?
      (make paragraph
	font-family-name: %title-font-family%
	font-weight: 'bold
	font-size: (HSIZE 4)
	line-spacing: (* (HSIZE 4) %line-spacing-factor%)
	space-before: (* (HSIZE 4) %head-before-factor%)
	space-after: (* (HSIZE 4) %head-after-factor%)
	start-indent: 0pt
	first-line-start-indent: 0pt
	quadding: %component-title-quadding%
	heading-level: (if %generate-heading-level% 1 0)
	keep-with-next?: #t
	(literal ($lot-title$ lotgi)))
      (empty-sosofo)))

;; Print the TOC entry for tocentry  
(define ($toc-entry$ tocentry level)
  (make paragraph
    start-indent: (+ %body-start-indent%
		     (* %toc-indent% level))
    first-line-start-indent: (* -1 %toc-indent%)
    font-weight: (if (= level 1) 'bold 'medium)
    space-before: (if (= level 1) (* %toc-spacing-factor% 6pt) 0pt)
    space-after: (if (= level 1) (* %toc-spacing-factor% 6pt) 0pt)
    keep-with-next?: (if (= level 1) #t #f)
    quadding: 'start
    (make link
      destination: (node-list-address tocentry)
      (make sequence
	(if (equal? (element-label tocentry) "")
	    (empty-sosofo)
	    (make sequence
	      (element-label-sosofo tocentry)
	      (literal (gentext-label-title-sep (gi tocentry)))))
	(element-title-sosofo tocentry)))
    (if (and (= level 1)
	     ;; ??? %chapter-title-page-separate%
	     %page-number-restart%)
	(empty-sosofo)    ;; Don't need the leader etc then
	(make sequence
	  (make leader (literal "."))
	  (make link
	    destination: (node-list-address tocentry)
	    (make sequence
	      (if %page-number-restart%
		  (literal
		   (string-append
		    (if (= level 1)
			(element-label tocentry #t)
			(substring (element-label tocentry #t)
				   0 (string-index (element-label tocentry #t) ".")))
		    (gentext-intra-label-sep "_pagenumber")))
		  (empty-sosofo))
	      (with-mode toc-page-number-mode
		(process-node-list tocentry))))))))

;; Build a TOC starting at nd reaching down depth levels.
;; The optional arguments are used on recursive calls to build-toc
;; and shouldn't be set by the initial caller...
;;
(define (build-toc nd depth #!optional (first? #t) (level 1))
  (let* ((toclist (toc-list-filter
		   (node-list-filter-by-gi (children nd)
					   (append (division-element-list)
						   (component-element-list)
						   (section-element-list))))))
    (if (or (<= depth 0)
	    (node-list-empty? toclist))
	(empty-sosofo)
	(make sequence
	  (toc-title first?)
	  (let loop ((nl toclist))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(sosofo-append
		  ($toc-entry$ (node-list-first nl) level)
		  (build-toc (node-list-first nl) (- depth 1) #f (+ level 1))
		  (loop (node-list-rest nl)))))))))

;; Print the LOT entry
(define ($lot-entry$ tocentry)
  (make paragraph
    start-indent: (+ %body-start-indent% %toc-indent%)
    first-line-start-indent: (* -1 %toc-indent%)
    font-weight: 'medium
    space-before: 0pt
    space-after: 0pt
    quadding: 'start
    (make link
      destination: (node-list-address tocentry)
      (make sequence
	(if (equal? (element-label tocentry) "")
	    (empty-sosofo)
	    (make sequence
	      (element-label-sosofo tocentry #t)
	      (literal (gentext-label-title-sep (gi tocentry)))))
	(element-title-sosofo tocentry)))
    (make leader (literal "."))
    (make link
      destination: (node-list-address tocentry)
      (make sequence
	(if %page-number-restart%
	    (make sequence
	     (literal (substring (element-label tocentry #t)
			0 (string-index (element-label tocentry #t) "-")))
	     (literal (gentext-intra-label-sep "_pagenumber")))
	    (empty-sosofo))
	(with-mode toc-page-number-mode
	  (process-node-list tocentry))))))

;; Build a LOT starting at nd for all the lotgi's it contains.
;; The optional arguments are used on recursive calls to build-toc
;; and shouldn't be set by the initial caller...
;;
(define (build-lot nd lotgi #!optional (first? #t))
  (let* ((lotlist (select-elements (descendants nd)
				   (normalize lotgi))))
    (if (node-list-empty? lotlist)
	(empty-sosofo)
	(make sequence
	  (lot-title first? lotgi)
	  (let loop ((nl lotlist))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(make sequence
		  (if (string=? (gi (node-list-first nl)) lotgi)
		      ($lot-entry$ (node-list-first nl))
		      (empty-sosofo))
		  (build-lot (node-list-first nl) lotgi #f)
		  (loop (node-list-rest nl)))))))))

(mode toc-page-number-mode
  (default
    (format-page-number)))
