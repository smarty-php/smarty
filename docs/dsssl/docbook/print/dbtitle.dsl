;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define title-style
  (style
   font-family-name: %title-font-family%
   font-weight: 'bold
   quadding: 'start))

;; So we can pass different sosofo's to this routine and get identical
;; treatment (see REFNAME in dbrfntry.dsl)
;;
(define ($lowtitlewithsosofo$ tlevel hlevel sosofo)
  (let ((hs (HSIZE (- 3 tlevel))))
    (make paragraph
	  font-family-name: %title-font-family%
	  font-weight: 'bold
	  font-size: hs
	  line-spacing: (* hs %line-spacing-factor%)
	  space-before: (* hs %head-before-factor%)
	  space-after: (* hs %head-after-factor%)
	  start-indent: %body-start-indent%
	  quadding: 'start
	  keep-with-next?: #t
	  heading-level: (if %generate-heading-level% hlevel 0)
	  sosofo)))

(define ($lowtitle$ tlevel hlevel)
  ($lowtitlewithsosofo$ tlevel hlevel (process-children)))

(define ($runinhead$)
  (let* ((title    (data (current-node)))
	 (titlelen (string-length title))
	 (lastchar (string-ref title (- titlelen 1)))
	 (punct    (if (member lastchar %content-title-end-punct%)
		       ""
		       %default-title-end-punct%)))
    (make sequence
      font-weight: 'bold
      (process-children)
      (literal punct " "))))

(element title ($lowtitle$ 2 4))         ;; the default TITLE format
(element titleabbrev (empty-sosofo))
(element subtitle (empty-sosofo))

(mode title-mode
  (element title
    (process-children)))
