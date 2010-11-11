;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;
;; Table support completely reimplemented by norm 15/16 Nov 1997
;;
;; ======================================================================
;;
;; This code is intended to implement the SGML Open Exchange Table Model
;; (http://www.sgmlopen.org/sgml/docs/techpubs.htm) as far as is possible
;; in RTF.  There are a few areas where this code probably fails to 
;; perfectly implement the model:
;;
;; - Mixed column width units (4*+2pi) are not supported.
;; - The behavior that results from mixing relative units with 
;;   absolute units has not been carefully considered.
;; - TFOOT appears at the bottom of the table, but is not repeated
;;   across the bottom of pages (RTF limitation).
;; - ENTRYTBL is not supported.
;; - Rotated tables (e.g. landscape tables in a portrait document)
;;   cannot be supported in a simple-page-sequence
;;
;; ======================================================================
;; 
;; My goal in reimplementing the table model was to provide correct
;; formatting in tables that use MOREROWS. The difficulty is that
;; correct formatting depends on calculating the column into which
;; an ENTRY will fall.
;;
;; This is a non-trivial problem because MOREROWS can hang down from
;; preceding rows and ENTRYs may specify starting columns (skipping
;; preceding ones).
;;
;; A simple, elegant recursive algorithm exists. Unfortunately it 
;; requires calculating the column number of every preceding cell 
;; in the entire table. Without memoization, performance is unacceptable
;; even in relatively small tables (5x5, for example).
;;
;; In order to avoid recursion, the algorithm used below is one that
;; works forward from the beginning of the table and "passes along"
;; the relevant information (column number of the preceding cell and
;; overhang from the MOREROWS in preceding rows).
;;
;; Unfortunately, this means that element construction rules
;; can't always be used to fire the appropriate rule.  Instead,
;; each TGROUP has to process each THEAD/BODY/FOOT explicitly.
;; And each of those must process each ROW explicitly, then each
;; ENTRY/ENTRYTBL explicitly.
;;
;; ----------------------------------------------------------------------
;;
;; I attempted to simplify this code by relying on inheritence from
;; table-column flow objects, but that wasn't entirely successful.
;; Horizontally spanning cells didn't seem to inherit from table-column
;; flow objects that didn't specify equal spanning.  There seemed to
;; be other problems as well, but they could have been caused by coding
;; errors on my part.
;; 
;; Anyway, by the time I understood how I could use table-column
;; flow objects for inheritence, I'd already implemented all the
;; machinery below to "work it out by hand".  
;;
;; ======================================================================
;; NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE 
;; ----------------------------------------------------------------------
;; A fairly large chunk of this code is in dbcommon.dsl!
;; ======================================================================
;;

;; Default value for FRAME= on tables
(define ($cals-frame-default$) (normalize "all"))

;; Default for COLSEP/ROWSEP if unspecified.
(define ($cals-rowsep-default$ #!optional (node (current-node)))
  ;; Return "0" for #f, "1" for #t
  ;; Default is to have rules if FRAME=ALL, otherwise not.  Except
  ;; that a separator between HEAD and BODY is controlled by
  ;; %table-head-body-border%.
  ;; 
  (let* ((table (ancestor-member node ($table-element-list$)))
	 (frame (if (attribute-string (normalize "frame") table)
		    (attribute-string (normalize "frame") table)
		    ($cals-frame-default$)))
	 (row   (ancestor-member node (list (normalize "row")))))
    (if (equal? frame (normalize "all"))
	#t
	(if (and (equal? (gi (parent row)) (normalize "thead"))
		 (last-sibling? row))
	    %table-head-body-border%
	    #f))))

(define ($cals-colsep-default$ #!optional (node (current-node)))
  ;; Default is to have rules if FRAME=ALL, otherwise not.
  ;;
  (let* ((table (ancestor-member node ($table-element-list$)))
	 (frame (if (attribute-string (normalize "frame") table)
		    (attribute-string (normalize "frame") table)
		    ($cals-frame-default$))))
    (equal? frame (normalize "all"))))

;; Default for VALIGN if unspecified
(define ($cals-valign-default$) (normalize "top"))

;; Margins around cell contents
(define %cals-cell-before-row-margin% 3pt)
(define %cals-cell-after-row-margin% 3pt)

;; seems to be a bug in JadeTeX -- we get a wierd indent on table
;;   cells for the first line only.  This is a workaround.
;; Adam Di Carlo, adam@onshore.com
(define %cals-cell-before-column-margin% 
  (cond ((equal? (print-backend) 'tex)
	 0pt)
	(else
	 3pt)))

(define %cals-cell-after-column-margin% 3pt)

;; Inheritable start and end indent for cell contents
(define %cals-cell-content-start-indent% 2pt)
(define %cals-cell-content-end-indent% 2pt)

;; How to indent pgwide tables?  (Non-pgwide tables get inherited-start-indent
(define %cals-pgwide-start-indent% %body-start-indent%)

;; What alignment should tables have on the page
(define %cals-display-align% 'start)

;; ----------------------------------------------------------------------
;; Table rule widths

(define %table-before-row-border% #t)
(define %table-after-row-border% #t)
(define %table-before-column-border% #t) 
(define %table-after-column-border% #t)
(define %table-head-body-border% #t)
(define %table-cell-after-column-border% #t)
(define %table-cell-after-row-border% #t)

;;(define tbl-color-space 
;;  (color-space "ISO/IEC 10179:1996//Color-Space Family::Device RGB"))
;;
;;(define tbl-red (color tbl-color-space 1 0 0))
;;(define tbl-green (color tbl-color-space 0 1 0))
;;(define tbl-blue (color tbl-color-space 0 0 1))

(define calc-table-before-row-border
  (if (boolean? %table-before-row-border%)
      %table-before-row-border%
      ;; Avoid problems with the DSSSL compiler when 
      ;; %table-before-row-border% is boolean.
      (let ((border-width %table-before-row-border%))
	(make table-border
	  line-thickness: border-width))))

(define calc-table-after-row-border
  (if (boolean? %table-after-row-border%)
      %table-after-row-border%
      (let ((border-width %table-after-row-border%))
	(make table-border
	  line-thickness: border-width))))

(define calc-table-before-column-border
  (if (boolean? %table-before-column-border%)
      %table-before-column-border%
      (let ((border-width %table-before-column-border%))
	(make table-border
	  line-thickness: border-width))))

(define calc-table-after-column-border
  (if (boolean? %table-after-column-border%)
      %table-after-column-border%
      (let ((border-width %table-after-column-border%))
	(make table-border
	  line-thickness: border-width))))

(define calc-table-head-body-border
  (if (boolean? %table-head-body-border%)
      %table-head-body-border%
      (let ((border-width %table-head-body-border%))
	(make table-border
	  line-thickness: border-width))))

(define calc-table-cell-after-column-border
  (if (boolean? %table-cell-after-column-border%)
      %table-cell-after-column-border%
      (let ((border-width %table-cell-after-column-border%))
	(make table-border
	  line-thickness: border-width))))

(define calc-table-cell-after-row-border
  (if (boolean? %table-cell-after-row-border%)
      %table-cell-after-row-border%
      (let ((border-width %table-cell-after-row-border%))
	(make table-border
	  line-thickness: border-width))))

;; ----------------------------------------------------------------------
;; Convert colwidth units into table-unit measurements

(define (colwidth-unit lenstr)
  (if (string? lenstr)
      (let ((number (length-string-number-part lenstr))
	    (units  (length-string-unit-part lenstr)))
	(if (string=? units "*")
	    (if (string=? number "")
		(table-unit 1)
		(table-unit (string->number number)))
	    (if (string=? units "")
		;; no units, default to points
		(* (string->number number) 1pt)
		(let* ((unum  (string->number number))
		       (uname (case-fold-down units)))
		  (case uname
		    (("mm") (* unum 1mm))
		    (("cm") (* unum 1cm))
		    (("in") (* unum 1in))
		    (("pi") (* unum 1pi))
		    (("pt") (* unum 1pt))
		    (("px") (* unum 1px))
		    ;; unrecognized units; use points
		    (else   (* unum 1pt)))))))
      ;; lenstr is not a string...probably #f
      (table-unit 1)))


(define (cell-align cell colnum)
  (let* ((entry     (ancestor-member cell (list (normalize "entry")
						(normalize "entrytbl"))))
	 (tgroup    (find-tgroup entry))
	 (spanname  (attribute-string (normalize "spanname") entry))
	 (calsalign (if (attribute-string (normalize "align") entry)
			(attribute-string (normalize "align") entry)
			(if (and spanname 
				 (spanspec-align (find-spanspec spanname)))
			    (spanspec-align (find-spanspec spanname))
			    (if (colspec-align (find-colspec-by-number colnum))
				(colspec-align (find-colspec-by-number colnum))
				(if (tgroup-align tgroup)
				    (tgroup-align tgroup)
				    (normalize "left")))))))
    (cond
     ((equal? calsalign (normalize "left")) 'start)
     ((equal? calsalign (normalize "center")) 'center)
     ((equal? calsalign (normalize "right")) 'end)
     (else 'start))))
    
(define (cell-valign cell colnum)
  (let* ((entry      (ancestor-member cell (list (normalize "entry")
						 (normalize "entrytbl"))))
	 (row        (ancestor (normalize "row") entry))
	 (tbody      (ancestor-member cell (list (normalize "tbody") 
						 (normalize "thead")
						 (normalize "tfoot"))))
	 (tgroup     (ancestor (normalize "tgroup") entry))
	 (calsvalign (if (attribute-string (normalize "valign") entry)
			 (attribute-string (normalize "valign") entry)
			 (if (attribute-string (normalize "valign") row)
			     (attribute-string (normalize "valign") row)
			     (if (attribute-string (normalize "valign") tbody)
				 (attribute-string (normalize "valign") tbody)
				 ($cals-valign-default$))))))
    (cond
     ((equal? calsvalign (normalize "top")) 'start)
     ((equal? calsvalign (normalize "middle")) 'center)
     ((equal? calsvalign (normalize "bottom")) 'end)
     (else 'start))))

;; ======================================================================
;; Element rules

(element tgroup
  (let ((frame-attribute (if (inherited-attribute-string (normalize "frame"))
			     (inherited-attribute-string (normalize "frame"))
			     ($cals-frame-default$))))
    (make table
      ;; These values are used for the outer edges (well, the top, bottom
      ;; and left edges for sure; I think the right edge actually comes
      ;; from the cells in the last column
      before-row-border:  (if (cond
			       ((equal? frame-attribute (normalize "all")) #t)
			       ((equal? frame-attribute (normalize "sides")) #f)
			       ((equal? frame-attribute (normalize "top")) #t)
			       ((equal? frame-attribute (normalize "bottom")) #f)
			       ((equal? frame-attribute (normalize "topbot")) #t)
			       ((equal? frame-attribute (normalize "none")) #f)
			       (else #f))
			      calc-table-before-row-border
			      #f)
      after-row-border:   (if (cond
			       ((equal? frame-attribute (normalize "all")) #t)
			       ((equal? frame-attribute (normalize "sides")) #f)
			       ((equal? frame-attribute (normalize "top")) #f)
			       ((equal? frame-attribute (normalize "bottom")) #t)
			       ((equal? frame-attribute (normalize "topbot")) #t)
			       ((equal? frame-attribute (normalize "none")) #f)
			       (else #f))
			      calc-table-after-row-border
			      #f)
      before-column-border: (if (cond
				 ((equal? frame-attribute (normalize "all")) #t)
				 ((equal? frame-attribute (normalize "sides")) #t)
				 ((equal? frame-attribute (normalize "top")) #f)
				 ((equal? frame-attribute (normalize "bottom")) #f)
				 ((equal? frame-attribute (normalize "topbot")) #f)
				 ((equal? frame-attribute (normalize "none")) #f)
				 (else #f))
				calc-table-before-column-border
				#f)
      after-column-border:  (if (cond
				 ((equal? frame-attribute (normalize "all")) #t)
				 ((equal? frame-attribute (normalize "sides")) #t)
				 ((equal? frame-attribute (normalize "top")) #f)
				 ((equal? frame-attribute (normalize "bottom")) #f)
				 ((equal? frame-attribute (normalize "topbot")) #f)
				 ((equal? frame-attribute (normalize "none")) #f)
				 (else #f))
				calc-table-after-column-border
				#f)
      display-alignment: %cals-display-align%
      (make table-part
	content-map: '((thead header)
		       (tbody #f)
		       (tfoot footer))
	($process-colspecs$ (current-node))
	(process-children)
	(make-table-endnotes)))))

(element colspec
  ;; now handled by $process-colspecs$ at the top of each tgroup...
  (empty-sosofo))

(element spanspec
  (empty-sosofo))

(element thead
  ($process-table-body$ (current-node)))

(element tfoot
  ($process-table-body$ (current-node)))

(element tbody
  ($process-table-body$ (current-node)))

(element row
  (empty-sosofo)) ;; this should never happen, they're processed explicitly

(element entry
  (empty-sosofo)) ;; this should never happen, they're processed explicitly

;; ======================================================================
;; Functions that handle processing of table bodies, rows, and cells

(define ($process-colspecs$ tgroup)
  (let* ((cols (string->number (attribute-string (normalize "cols")))))
    (let loop ((colnum 1))
      (if (> colnum cols)
	  (empty-sosofo)
	  (make sequence
	    (let ((colspec (find-colspec-by-number colnum)))
	      (if (node-list-empty? colspec)
		  (make table-column
		    column-number: colnum
		    width: (colwidth-unit "1*"))
		  ($process-colspec$ colspec colnum)))
	    (loop (+ colnum 1)))))))

(define ($process-colspec$ colspec colnum)
  (let* ((colwidth (if (attribute-string (normalize "colwidth") colspec)
		       (attribute-string (normalize "colwidth") colspec)
		       "1*")))
    (make table-column
      column-number: colnum
      width: (colwidth-unit colwidth))))

(define ($process-table-body$ body)
  (let* ((tgroup (ancestor (normalize "tgroup") body))
	 (cols   (string->number (attribute-string (normalize "cols") tgroup)))
	 (blabel (cond 
		   ((equal? (gi body) (normalize "thead")) 'thead)
		   ((equal? (gi body) (normalize "tbody")) 'tbody)
		   ((equal? (gi body) (normalize "tfoot")) 'tfoot))))
    (make sequence
      label: blabel
      (let loop ((rows (select-elements (children body) (normalize "row")))
		 (overhang (constant-list 0 cols)))
	(if (node-list-empty? rows)
	    (empty-sosofo)
	    (make sequence
	      ($process-row$ (node-list-first rows) overhang)
	      (loop (node-list-rest rows)
		    (update-overhang (node-list-first rows) overhang))))))))

(define ($process-row$ row overhang)
  (let* ((tgroup           (ancestor (normalize "tgroup") row))
	 (maxcol           (string->number (attribute-string 
					    (normalize "cols") tgroup)))
	 (lastentry        (node-list-last (node-list-filter-out-pis 
					    (children row))))
	 (table            (parent tgroup)))
    ;; there's no point calculating the row or colsep here, each cell
    ;; specifies it which overrides anything we might say here...
    (make table-row
      (let loop ((cells (node-list-filter-out-pis (children row)))
		 (prevcell (empty-node-list)))
	(if (node-list-empty? cells)
	    (empty-sosofo)
	    (make sequence
	      ($process-cell$ (node-list-first cells) prevcell row overhang)
	      (loop (node-list-rest cells) (node-list-first cells)))))
      
      ;; add any necessary empty cells to the end of the row
      (let loop ((colnum (+ (cell-column-number lastentry overhang)
			    (hspan lastentry))))
	(if (> colnum maxcol)
	    (empty-sosofo)
	    (make sequence
	      ($process-empty-cell$ colnum row)
	      (loop (+ colnum 1))))))))

(define ($process-cell$ entry preventry row overhang)
  (let* ((colnum    (cell-column-number entry overhang))
	 (lastcellcolumn (if (node-list-empty? preventry)
			     0
			     (- (+ (cell-column-number preventry overhang)
				   (hspan preventry))
				1)))
	 (lastcolnum (if (> lastcellcolumn 0)
			 (overhang-skip overhang lastcellcolumn)
			 0))
	 (font-name (if (have-ancestor? (normalize "thead") entry)
			%title-font-family%
			%body-font-family%))
	 (weight    (if (have-ancestor? (normalize "thead") entry)
			'bold
			'medium))
	 (align     (cell-align entry colnum)))

    (make sequence
      ;; This is a little bit complicated.  We want to output empty cells
      ;; to skip over missing data.  We start count at the column number
      ;; arrived at by adding 1 to the column number of the previous entry
      ;; and skipping over any MOREROWS overhanging entrys.  Then for each
      ;; iteration, we add 1 and skip over any overhanging entrys.
      (let loop ((count (overhang-skip overhang (+ lastcolnum 1))))
	(if (>= count colnum)
	    (empty-sosofo)
	    (make sequence
	      ($process-empty-cell$ count row)
	      (loop (overhang-skip overhang (+ count 1))))))

      ;; Now we've output empty cells for any missing entries, so we 
      ;; are ready to output the cell for this entry...
      (make table-cell 
	column-number: colnum
	n-columns-spanned: (hspan entry)
	n-rows-spanned: (vspan entry)

	cell-row-alignment: (cell-valign entry colnum)

	cell-after-column-border: (if (cell-colsep entry colnum)
				      calc-table-cell-after-column-border
				      #f)

	cell-after-row-border: (if (cell-rowsep entry colnum)
				   (if (last-sibling? (parent entry))
				       calc-table-head-body-border
				       calc-table-cell-after-row-border)
				   #f)

	cell-before-row-margin: %cals-cell-before-row-margin%
	cell-after-row-margin: %cals-cell-after-row-margin%
	cell-before-column-margin: %cals-cell-before-column-margin%
	cell-after-column-margin: %cals-cell-after-column-margin%

	;; If there is some additional indentation (because we're in a list,
	;; for example) make sure that gets passed along, but don't add
	;; the normal body-start-indent.
	start-indent: (+ (- (inherited-start-indent) %body-start-indent%)
			 %cals-cell-content-start-indent%)
	end-indent: %cals-cell-content-end-indent%
	(if (equal? (gi entry) (normalize "entrytbl"))
	    (make paragraph 
	      (literal "ENTRYTBL not supported."))
	    (make paragraph
	      font-family-name: font-name
	      font-weight: weight
	      quadding: align
	      (process-node-list (children entry))))))))

(define (empty-cell-colsep colnum row)
  (let* ((tgroup    (ancestor (normalize "tgroup") row))
	 (table     (parent tgroup))
	 (calscolsep 
	  (if (tgroup-colsep tgroup)
	      (tgroup-colsep tgroup)
	      (if (attribute-string (normalize "colsep") table)
		  (attribute-string (normalize "colsep") table)
		  (if ($cals-colsep-default$ row)
		      "1"
		      "0")))))
    (> (string->number calscolsep) 0)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (cell-colsep cell colnum)
  (let* ((entry     (ancestor-member cell (list (normalize "entry") (normalize "entrytbl"))))
	 (spanname  (attribute-string (normalize "spanname") entry))
	 (tgroup    (find-tgroup entry))
	 (table     (parent tgroup))
	 (calscolsep 
	  (if (attribute-string (normalize "colsep") entry)
	      (attribute-string (normalize "colsep") entry)
	      (if (and spanname 
		       (spanspec-colsep (find-spanspec spanname)))
		  (spanspec-colsep (find-spanspec spanname))
		  (if (colspec-colsep (find-colspec-by-number colnum))
		      (colspec-colsep (find-colspec-by-number colnum))
		      (if (tgroup-colsep tgroup)
			  (tgroup-colsep tgroup)
			  (if (attribute-string (normalize "colsep") table)
			      (attribute-string (normalize "colsep") table)
			      (if ($cals-colsep-default$ cell)
				  "1"
				  "0"))))))))
    (> (string->number calscolsep) 0)))

(define (cell-rowsep cell colnum)
  (let* ((entry     (ancestor-member cell (list (normalize "entry") 
						(normalize "entrytbl"))))
	 (spanname  (attribute-string (normalize "spanname") entry))
	 (row       (ancestor (normalize "row") entry))
	 (tgroup    (find-tgroup entry))
	 (table     (parent tgroup))
	 (calsrowsep 
	  (if (attribute-string (normalize "rowsep") entry)
	      (attribute-string (normalize "rowsep") entry)
	      (if (and spanname (spanspec-rowsep (find-spanspec spanname)))
		  (spanspec-rowsep (find-spanspec spanname))
		  (if (colspec-rowsep (find-colspec-by-number colnum))
		      (colspec-rowsep (find-colspec-by-number colnum))
		      (if (attribute-string (normalize "rowsep") row)
			  (attribute-string (normalize "rowsep") row)
			  (if (tgroup-rowsep tgroup)
			      (tgroup-rowsep tgroup)
			      (if (attribute-string (normalize "rowsep") table)
				  (attribute-string (normalize "rowsep") table)
				  (if ($cals-rowsep-default$ cell)
				      "1"
				      "0")))))))))
    (> (string->number calsrowsep) 0)))

(define (empty-cell-rowsep colnum row)
  (let* ((tgroup    (ancestor (normalize "tgroup") row))
	 (table     (parent tgroup))
	 (calsrowsep 
	  (if (attribute-string (normalize "rowsep") row)
	      (attribute-string (normalize "rowsep") row)
	      (if (tgroup-rowsep tgroup)
		  (tgroup-rowsep tgroup)
		  (if (attribute-string (normalize "rowsep") table)
		      (attribute-string (normalize "rowsep") table)
		      (if ($cals-rowsep-default$ row)
			  "1"
			  "0"))))))
    (> (string->number calsrowsep) 0)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define ($process-empty-cell$ colnum row)
  (make table-cell 
    column-number: colnum
    n-columns-spanned: 1
    n-rows-spanned: 1
    cell-after-column-border: (if (empty-cell-colsep colnum row)
				  calc-table-cell-after-column-border
				  #f)
    
    cell-after-row-border: (if (empty-cell-rowsep colnum row)
			       (if (last-sibling? row)
				   calc-table-head-body-border
				   calc-table-cell-after-row-border)
			       #f)
    
    cell-before-row-margin: %cals-cell-before-row-margin%
    cell-after-row-margin: %cals-cell-after-row-margin%
    cell-before-column-margin: %cals-cell-before-column-margin%
    cell-after-column-margin: %cals-cell-after-column-margin%
    start-indent: %cals-cell-content-start-indent%
    end-indent: %cals-cell-content-end-indent%
    (empty-sosofo)))

;; EOF
