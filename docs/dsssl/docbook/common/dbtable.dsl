;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;
;; This file contains table functions common to both print and HTML
;; versions of the DocBook stylesheets.
;;

;; If **ANY** change is made to this file, you _MUST_ alter the
;; following definition:

(define %docbook-common-table-version%
  "Modular DocBook Stylesheet Common Table Functions")

;; == Table Support =====================================================

;; ----------------------------------------------------------------------
;; Functions for finding/retrieving table attributes

(define (tgroup-align tgroup)
  (attribute-string (normalize "align") tgroup))

(define (tgroup-colsep tgroup)
  (attribute-string (normalize "colsep") tgroup))

(define (tgroup-rowsep tgroup)
  (attribute-string (normalize "rowsep") tgroup))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (find-tgroup #!optional (nd (current-node)))
  ;; for our purposes, an entrytbl functions as a tgroup.
  ;; ENTRYTBL IS ONLY SUPPORTED IN THE HTML BACKEND!!!
  (if (or (equal? (gi nd) (normalize "tgroup"))
	  (equal? (gi nd) (normalize "entrytbl")))
      nd
      (if (node-list-empty? (ancestor (normalize "entrytbl") nd))
	  (ancestor (normalize "tgroup") nd)
	  (ancestor (normalize "entrytbl") nd))))

(define (find-colspec colname)
  (let* ((tgroup (find-tgroup))
	 (colspecs (select-elements (descendants tgroup)
				    (normalize "colspec"))))
    (let loop ((nl colspecs))
	(if (node-list-empty? nl)
	    ;; we've run out of places to look, stop looking...
	    (error (string-append "Could not find COLSPEC named " colname))
	    (if (equal? colname
			(attribute-string (normalize "colname") 
					  (node-list-first nl)))
		(node-list-first nl)
		(loop (node-list-rest nl)))))))

(define (find-colspec-by-number colnum)
  (let* ((tgroup (find-tgroup))
	 (colspecs (select-elements (children tgroup) (normalize "colspec"))))
    (let loop ((nl colspecs))
      (if (node-list-empty? nl)
	  ;; we've run out of places to look, stop looking...
	  (empty-node-list)
	  (if (equal? (colspec-colnum (node-list-first nl)) colnum)
	      (node-list-first nl)
	      (loop (node-list-rest nl)))))))

(define (colspec-align colspec)
  (attribute-string (normalize "align") colspec))

(define (colspec-char colspec)
  (attribute-string (normalize "char") colspec))

(define (colspec-charoff colspec)
  (let ((charoff (attribute-string (normalize "charoff") colspec)))
    (if charoff
	(string->number charoff)
	#f)))

(define (colspec-colnum colspec)
  ;; returns the column number of the associated colspec...which is 
  ;; either the value of COLNUM or obtained by counting
  (let* ((tgroup (find-tgroup colspec))
	 (colspecs (select-elements (children tgroup) (normalize "colspec"))))
    (if (attribute-string (normalize "colnum") colspec)
	(string->number (attribute-string (normalize "colnum") colspec))
	(let loop ((nl colspecs) (curcol 1))
	  (let ((colnum (attribute-string (normalize "colnum") (node-list-first nl))))
	    (if (node-list=? (node-list-first nl) colspec)
		curcol
		(if colnum
		    (loop (node-list-rest nl) (+ (string->number colnum) 1))
		    (loop (node-list-rest nl) (+ curcol 1)))))))))

(define (colspec-colname colspec)
  (attribute-string (normalize "colname") colspec))

(define (colspec-colsep colspec)
  (attribute-string (normalize "colsep") colspec))

(define (colspec-colwidth colspec)
  (if (attribute-string (normalize "colwidth") colspec)
      (attribute-string (normalize "colwidth") colspec)
      "1*"))

(define (colspec-rowsep colspec)
  (attribute-string (normalize "rowsep") colspec))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (find-spanspec spanname)
  (let* ((tgroup (find-tgroup))
	 (spanspecs (select-elements (descendants tgroup) 
				     (normalize (normalize "spanspec")))))
    (let loop ((nl spanspecs))
      (if (node-list-empty? nl)
	  (error (string-append "Could not find SPANSPEC named " spanname))
	  (if (equal? spanname 
		      (attribute-string (normalize "spanname")
					(node-list-first nl)))
	      (node-list-first nl)
	      (loop (node-list-rest nl)))))))

(define (spanspec-align spanspec)
  (attribute-string (normalize "align") spanspec))

(define (spanspec-char spanspec)
  (attribute-string (normalize "char") spanspec))

(define (spanspec-charoff spanspec)
  (let ((charoff (attribute-string (normalize "charoff") spanspec)))
    (if charoff
	(string->number charoff)
	#f)))

(define (spanspec-colsep spanspec)
  (attribute-string (normalize "colsep") spanspec))

(define (spanspec-nameend spanspec)
  (attribute-string (normalize "nameend") spanspec))

(define (spanspec-namest spanspec)
  (attribute-string (normalize "namest") spanspec))

(define (spanspec-rowsep spanspec)
  (attribute-string (normalize "rowsep") spanspec))

(define (spanspec-spanname spanspec)
  (attribute-string (normalize "spanname") spanspec))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Calculate spans

(define (hspan entry)
  ;; Returns the horizontal span of an entry
  (let* ((spanname (attribute-string (normalize "spanname") entry))
	 (namest   (if spanname
		       (spanspec-namest (find-spanspec spanname))
		       (attribute-string (normalize "namest") entry)))
	 (nameend  (if spanname
		       (spanspec-nameend (find-spanspec spanname))
		       (attribute-string (normalize "nameend") entry)))
	 (colst    (if namest
		       (colspec-colnum (find-colspec namest))
		       #f))
	 (colend   (if nameend
		       (colspec-colnum (find-colspec nameend))
		       #f)))
    (if (and namest nameend)
	(+ (- colend colst) 1)
	1)))

(define (vspan entry)
  ;; Returns the vertical span of an entry.  Note that this is one more
  ;; than the specified MOREROWS attribute.
  (let* ((morerows (attribute-string (normalize "morerows") entry)))
    (if morerows
	(+ (string->number morerows) 1)
	1)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Update the "overhang" list

(define (adjust-overhang overhang oldoverhang entry)
  (let* ((colst    (cell-column-number entry oldoverhang))
	 (span     (hspan entry)))
    (if (> (vspan entry) 1)
	(list-put overhang colst (- (vspan entry) 1) span)
	overhang)))

(define (overhang-skip overhang startcol)
  (if (> startcol (length overhang))
      ;; this is a _broken_ table.  should I output a debug message!?
      startcol
      (let loop ((overtail (list-tail overhang (- startcol 1))) (col startcol))
	(if (null? overtail)
	    col
	    (if (equal? (car overtail) 0)
		col
		(loop (cdr overtail) (+ col 1)))))))

(define (update-overhang row oldoverhang)
  (let loop ((overhang (decrement-list-members oldoverhang))
	     (entries  (node-list-filter-out-pis (children row))))
    (if (node-list-empty? entries)
	overhang
	(loop (adjust-overhang overhang oldoverhang 
			       (node-list-first entries))
	      (node-list-rest entries)))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Calculate information about cells

(define (cell-prev-cell entry)
  ;; Return the cell which precedes entry in the current row.
  (let loop ((nd (ipreced entry)))
    (if (node-list-empty? nd)
	nd
	(if (equal? (node-property 'class-name nd) 'element)
	    nd
	    (loop (ipreced nd))))))

(define (cell-column-number entry overhang)
  (let* ((entry     (ancestor-member entry (list (normalize "entry") (normalize "entrytbl"))))
	 (row       (ancestor (normalize "row") entry))
	 (preventry (cell-prev-cell entry))
	 (prevspan  (if (node-list-empty? preventry) 1 (hspan preventry)))
	 (colname   (attribute-string (normalize "colname") entry))
	 (namest    (attribute-string (normalize "namest") entry))
	 (nameend   (attribute-string (normalize "nameend") entry))
	 (spanname  (attribute-string (normalize "spanname") entry)))
    (if colname
	(colspec-colnum (find-colspec colname))
	(if spanname
	    (colspec-colnum (find-colspec 
			     (spanspec-namest (find-spanspec spanname))))
	    (if namest
		(colspec-colnum (find-colspec namest))
		(if (node-list-empty? preventry)
		    (overhang-skip overhang 1)
		    (overhang-skip overhang 
				   (+ (cell-column-number preventry overhang) 
				   prevspan))))))))

;; ======================================================================
