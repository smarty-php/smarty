;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ============================== INLINES ===============================

(element accel ($score-seq$ 'after))
(element action ($charseq$))
(element application ($charseq$))
(element classname ($mono-seq$))
(element command ($bold-seq$))
(element computeroutput ($mono-seq$))
(element database ($charseq$))
(element email 
  (make sequence (literal "&#60;") ($mono-seq$) (literal "&#62;")))
(element envar ($charseq$))
(element errorcode ($charseq$))
(element errorname ($charseq$))
(element errortype ($charseq$))
(element filename ($mono-seq$))
(element function ($mono-seq$))
(element guibutton ($guilabel-seq$))
(element guiicon ($guilabel-seq$))
(element guilabel ($guilabel-seq$))
(element guimenu ($guilabel-seq$))
(element guimenuitem ($guilabel-seq$))
(element guisubmenu ($guilabel-seq$))
(element hardware ($charseq$))
(element interface ($charseq$))
(element interfacedefinition ($charseq$))
(element keycap ($bold-seq$))
(element keycode ($charseq$))

(element keycombo 
  (let* ((action (attribute-string (normalize "action")))
	 (joinchar 
	  (cond
	   ((equal? action (normalize "seq")) " ")          ;; space
	   ((equal? action (normalize "simul")) "+")        ;; +
	   ((equal? action (normalize "press")) "-")        ;; ? I don't know
	   ((equal? action (normalize "click")) "-")        ;; ? what to do
	   ((equal? action (normalize "double-click")) "-") ;; ? about the rest
	   ((equal? action (normalize "other")) "-")        ;; ? of these
	   (else "-"))))
    (let loop ((nl (children (current-node))) (count 1))
      (if (node-list-empty? nl)
	  (empty-sosofo)
	  (if (equal? count 1)
	      (make sequence
		(process-node-list (node-list-first nl))
		(loop (node-list-rest nl) (+ count 1)))
	      (make sequence
		(literal joinchar)
		(process-node-list (node-list-first nl))
		(loop (node-list-rest nl) (+ count 1))))))))

(element keysym ($charseq$))
(element literal ($mono-seq$))
(element medialabel ($italic-seq$))

(element menuchoice
  (let* ((shortcut (select-elements (children (current-node)) 
				    (normalize "shortcut")))
	 (items    (node-list-filter-by-not-gi
		    (children (current-node))
		    (list (normalize "shortcut")))))
    (make sequence
      (let loop ((nl items) (first? #t))
	(if (node-list-empty? nl)
	    (empty-sosofo)
	    (make sequence
	      (if first?
		  (process-node-list (node-list-first nl))
		  (make sequence
		    (if (or (equal? (gi (node-list-first nl))
				    (normalize "guimenuitem"))
			    (equal? (gi (node-list-first nl))
				    (normalize "guisubmenu")))
			(literal "\rightwards-arrow;")
			(literal "+"))
		    (process-node-list (node-list-first nl))))
	      (loop (node-list-rest nl) #f))))
      (if (node-list-empty? shortcut)
	  (empty-sosofo)
	  (make sequence
	    (literal " (")
	    (process-node-list shortcut)
	    (literal ")"))))))

(element methodname ($mono-seq$))
(element shortcut ($bold-seq$))
(element mousebutton ($charseq$))
(element option ($mono-seq$))

(element optional 
  (make sequence 
    (literal %arg-choice-opt-open-str%)
    ($charseq$)
    (literal %arg-choice-opt-close-str%)))

(element parameter ($italic-mono-seq$))
(element property ($charseq$))
(element prompt ($mono-seq$))
(element replaceable ($italic-mono-seq$))
(element returnvalue ($charseq$))
(element structfield ($italic-mono-seq$))
(element structname ($charseq$))
(element symbol ($charseq$))
(element systemitem ($charseq$))
(element token ($charseq$))
(element type ($charseq$))
(element userinput ($bold-mono-seq$))
(element abbrev ($charseq$))
(element acronym ($charseq$))

(element citation 
  (if biblio-citation-check
      (let* ((bgraphies (select-elements (descendants (sgml-root-element))
					 (normalize "bibliography")))
	     (bchildren1 (expand-children bgraphies
					  (list (normalize "bibliography"))))
	     (bchildren2 (expand-children bchildren1
					  (list (normalize "bibliodiv"))))
	     (bibentries (node-list-filter-by-gi 
			  bchildren2
			  (list (normalize "biblioentry")
				(normalize "bibliomixed")))))
	(let loop ((bibs bibentries))
	  (if (node-list-empty? bibs)
	      (make sequence
		(error (string-append "Cannot find citation: " 
					   (data (current-node))))
		(literal "[") ($charseq$) (literal "]"))
	      (if (citation-matches-target? (current-node) 
					    (node-list-first bibs))
		  (make link 
		    destination: (node-list-address (node-list-first bibs))
		    (literal "[") ($charseq$) (literal "]"))
		  (loop (node-list-rest bibs))))))
      (make sequence 
	(literal "[") ($charseq$) (literal "]"))))

(element citerefentry 
  (if %refentry-xref-italic%
      ($italic-seq$)
      ($charseq$)))

(element citetitle
  (if (equal? (attribute-string (normalize "pubwork")) "article")
      (make sequence
	(literal (gentext-start-quote))
	(process-children)
	(literal (gentext-end-quote)))
      ($italic-seq$)))

(element emphasis
  (if (and (attribute-string (normalize "role"))
	   (or (equal? (attribute-string (normalize "role")) "strong")
	       (equal? (attribute-string (normalize "role")) "bold")))
      ($bold-seq$)
      ($italic-seq$)))

(element foreignphrase ($italic-seq$))
(element markup ($charseq$))
(element phrase ($charseq$))

(element quote
  (let* ((hnr   (hierarchical-number-recursive (normalize "quote") 
					       (current-node)))
	 (depth (length hnr)))
    (if (equal? (modulo depth 2) 1)
	(make sequence
	  (literal (gentext-start-nested-quote))
	  (process-children)
	  (literal (gentext-end-nested-quote)))
	(make sequence
	  (literal (gentext-start-quote))
	  (process-children)
	  (literal (gentext-end-quote))))))

(element sgmltag
  (let ((class (if (attribute-string (normalize "class"))
		   (attribute-string (normalize "class"))
		   (normalize "element"))))
    (cond
<![CDATA[
      ((equal? class (normalize "attribute")) ($mono-seq$))
      ((equal? class (normalize "attvalue")) ($mono-seq$))
      ((equal? class (normalize "element")) ($mono-seq$))
      ((equal? class (normalize "emptytag")) ($mono-seq$ (make sequence 
				  (literal "<") 
				  (process-children)
				  (literal "/>"))))
      ((equal? class (normalize "endtag")) ($mono-seq$ (make sequence 
			       (literal "</") 
			       (process-children)
			       (literal ">"))))
      ((equal? class (normalize "genentity")) ($mono-seq$ (make sequence
				    (literal "&")
				    (process-children)
				    (literal ";"))))
      ((equal? class (normalize "numcharref")) ($mono-seq$ (make sequence
				     (literal "&#")
				     (process-children)
				     (literal ";"))))
      ((equal? class (normalize "paramentity")) ($mono-seq$ (make sequence
				      (literal "%")
				      (process-children)
				      (literal ";"))))
      ((equal? class (normalize "pi")) ($mono-seq$ (make sequence 
			    (literal "<?")
			    (process-children)
			    (literal ">"))))
      ((equal? class (normalize "starttag")) ($mono-seq$ (make sequence 
				  (literal "<") 
				  (process-children)
				  (literal ">"))))
      ((equal? class (normalize "sgmlcomment")) ($mono-seq$ (make sequence 
				     (literal "<!--")
				     (process-children)
				     (literal "-->"))))
      ((equal? class (normalize "xmlpi")) ($mono-seq$ (make sequence 
			    (literal "<?")
			    (process-children)
			    (literal "?>"))))
]]>
      (else ($charseq$)))))

(element trademark 
  (make sequence
    ($charseq$)
    (cond
     ((equal? (attribute-string "class") (normalize "copyright"))
      (literal "\copyright-sign;"))
     ((equal? (attribute-string "class") (normalize "registered"))
      (literal "\registered-sign;"))
     ((equal? (attribute-string "class") (normalize "service"))
      ($ss-seq$ + (literal "SM")))
     (else
      (literal "\trade-mark-sign;")))))

(element wordasword ($italic-seq$))

(element lineannotation
  (make sequence
    font-family-name: %body-font-family%
    font-posture: 'italic
    (process-children)))

(define ($ss-seq$ plus-or-minus #!optional (sosofo (process-children)))
  (make sequence
	font-size:
	  (* (inherited-font-size) %ss-size-factor%)
	position-point-shift:
	  (plus-or-minus (* (inherited-font-size) %ss-shift-factor%))
	sosofo))

(element superscript ($ss-seq$ +))
(element subscript ($ss-seq$ -))
