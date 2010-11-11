;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

(define (pi-value component piname)
  ;; Returns the value of the (?piname value) PI (if one exists)
  ;; as a child of component, otherwise returns #f
  ;;
  (let loop ((nl (select-by-class (children component) 'pi)))
    (if (node-list-empty? nl)
	#f
	(let ((pidata (node-property 'system-data (node-list-first nl))))
	  (if (and (> (string-length pidata) (string-length piname))
		   (equal? piname
			   (substring pidata 0 (string-length piname))))
	      (substring pidata
			 (+ (string-length piname) 1)
			 (string-length pidata))
	      (loop (node-list-rest nl)))))))

(define (inherited-pi-value component piname)
  (let loop ((value #f) (nd component))
    (if (or value (node-list-empty? nd))
	value
	(loop (pi-value nd piname) (parent nd)))))

(define (dbhtml-findvalue pi-field-list name)
  ;; pi-field-list is '(pitarget name1 value1 name2 value2 ...)
  (let loop ((slist (cdr pi-field-list)))
    (if (null? slist)
	#f
	(if (string=? (car slist) name)
	    (car (cdr slist))
	    (loop (cdr (cdr slist)))))))

(define (dbhtml-value component name)
  ;; Returns the value of "name='value'" in the &#60;?dbhtml ...> PI
  (let loop ((nl (select-by-class (children component) 'pi)))
    (if (node-list-empty? nl)
	#f
	(let* ((pidata (node-property 'system-data (node-list-first nl)))
	       (pilist (if (and (> (string-length pidata) 7)
				(string=? (substring pidata 0 7) "dbhtml "))
			   (parse-starttag-pi pidata)
			   '()))
	       (value  (if (null? pilist) #f (dbhtml-findvalue pilist name))))
	  (if value
	      value
	      (loop (node-list-rest nl)))))))

(define (inherited-dbhtml-value component name)
  (let loop ((value #f) (nd component))
    (if (or value (node-list-empty? nd))
	value
	(loop (dbhtml-value nd name) (parent nd)))))

;; EOF dbpi.dsl


