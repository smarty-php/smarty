;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; So we can pass different sosofo's to this routine and get identical
;; treatment (see REFNAME in dbrfntry.dsl)
;;
(define ($lowtitlewithsosofo$ tlevel sosofo)
  (let ((tgi (cond
	      ((equal? tlevel 1) "H1")
	      ((equal? tlevel 2) "H2")
	      ((equal? tlevel 3) "H3")
	      ((equal? tlevel 4) "H4")
	      ((equal? tlevel 5) "H5")
	      (else "P"))))
    (if (< tlevel 6)
	(make element gi: tgi
	      sosofo)
	(make element gi: "P"
	      (make element gi: "B"
		    sosofo)))))

(define ($lowtitle$ tlevel)
  ($lowtitlewithsosofo$ tlevel (process-children)))

(define ($runinhead$)
  (let* ((title    (data (current-node)))
	 (titlelen (string-length title))
	 (lastchar (if (> titlelen 0)
		       (string-ref title (- titlelen 1))
		       "."))
	 (punct    (if (or (= titlelen 0) 
			   (member lastchar %content-title-end-punct%))
		       ""
		       %default-title-end-punct%)))
    (make element gi: "B"
	  (process-children)
	  (literal punct " "))))

(element title 
  (make element gi: "P"
	(make element gi: "B"
	      (process-children-trim))))

(element titleabbrev (empty-sosofo))

(mode title-mode
  (element title
    (process-children)))

(mode subtitle-mode
  (element subtitle
    (make sequence
      (literal (if (first-sibling?)
		   ""
		   "; "))
      (process-children))))

(mode head-title-mode
  ;; TITLE in an HTML HEAD
  (default
    (process-children))

  (element graphic (empty-sosofo))
  (element inlinegraphic (empty-sosofo)))
