;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(define verbatim-style
  (style
      font-family-name: %mono-font-family%
      font-size:        (* (inherited-font-size) 
			   (if %verbatim-size-factor%
			       %verbatim-size-factor%
			       1.0))
      font-weight:      'medium
      font-posture:     'upright
      line-spacing:     (* (* (inherited-font-size) 
			      (if %verbatim-size-factor%
				  %verbatim-size-factor%
				  1.0))
			   %line-spacing-factor%)
      first-line-start-indent: 0pt
      lines: 'asis
      input-whitespace-treatment: 'preserve))

(define inline-verbatim-style
  (style
      font-family-name: %mono-font-family%
      font-size:        (* (inherited-font-size) 
			   (if %verbatim-size-factor%
			       %verbatim-size-factor%
			       1.0))
      font-weight:      'medium
      font-posture:     'upright
      lines: 'asis
      input-whitespace-treatment: 'preserve))

(define linespecific-style
  (style
      first-line-start-indent: 0pt
      lines: 'asis
      input-whitespace-treatment: 'preserve))

(define ($format-indent$ indent)
  (literal indent))

(define ($format-linenumber$ linenumber)
  ;; A line-field would make more sense here, and allow proportional
  ;; fonts, but you can't put line-fields in the middle of a paragraph
  ;; in the current RTF backend of Jade
  (let ((%factor% (if %verbatim-size-factor%
		      %verbatim-size-factor%
		      1.0)))
    (if (equal? (remainder linenumber %linenumber-mod%) 0)
	(make sequence
	  use: verbatim-style
	  (literal (pad-string (format-number linenumber "1") 
			       %linenumber-length% %linenumber-padchar%))
	  ($linenumber-space$))
	(make sequence
	  use: verbatim-style
	  (literal (pad-string "" %linenumber-length% " "))
	  ($linenumber-space$)))))

(define ($line-start$ indent line-numbers? #!optional (line-number 1))
  (make sequence
    (if indent
	($format-indent$ indent)
	(empty-sosofo))
    (if line-numbers?
	($format-linenumber$ line-number)
	(empty-sosofo))))

(define ($linespecific-display$ indent line-numbers?)
  (let ((vspace (if (INBLOCK?)
		   0pt
		   (if (INLIST?) 
		       %para-sep% 
		       %block-sep%))))
    (make paragraph
      use: linespecific-style
      space-before: (if (and (string=? (gi (parent)) (normalize "entry"))
 			     (absolute-first-sibling?))
			0pt
			vspace)
      space-after:  (if (and (string=? (gi (parent)) (normalize "entry"))
 			     (absolute-last-sibling?))
			0pt
			vspace)
      start-indent: (if (INBLOCK?)
			(inherited-start-indent)
			(+ %block-start-indent% (inherited-start-indent)))
      (if (or indent line-numbers?)
	  ($linespecific-line-by-line$ indent line-numbers?)
	  (process-children)))))

(define ($linespecific-line-by-line$ indent line-numbers?)
  (let ((expanded-content
	 ;; this is the content with
	 ;; inlinemediaobject/imageobject[@format='linespecific']
	 ;; expanded
	 (let loop ((kl (children (current-node))) (rl (empty-node-list)))
	   (if (node-list-empty? kl)
	       rl
	       (if (equal? (gi (node-list-first kl))
			   (normalize "inlinemediaobject"))
		   (let* ((imgobj (node-list-filter-by-gi
				   (children (node-list-first kl))
				   (list (normalize "imageobject"))))
			  (datobj (node-list-filter-by-gi
				   (children imgobj)
				   (list (normalize "imagedata")))))
		     (if (and (not (node-list-empty? imgobj))
			      (not (node-list-empty? datobj))
			      (equal? (attribute-string (normalize "format") datobj)
				      (normalize "linespecific")))
			 (loop (node-list-rest kl)
			       (node-list rl (string->nodes (include-characters
							     (if (attribute-string (normalize "fileref") datobj)
								 (attribute-string (normalize "fileref") datobj)
								 (entity-generated-system-id (attribute-string (normalize "entityref") datobj)))))))
			 (loop (node-list-rest kl)
			       (node-list rl (node-list-first kl)))))
		   (loop (node-list-rest kl) (node-list rl (node-list-first kl))))))))
  (make sequence
    ($line-start$ indent line-numbers? 1)
    (let loop ((kl expanded-content)
	       (linecount 1)
	       (res (empty-sosofo)))
      (if (node-list-empty? kl)
	  res
	  (loop
	   (node-list-rest kl)
	   (if (char=? (node-property 'char (node-list-first kl)
				      default: #\U-0000) #\U-000D)
	       (+ linecount 1)
	       linecount)
	   (let ((c (node-list-first kl)))
	     (if (char=? (node-property 'char c default: #\U-0000) 
			 #\U-000D)
		 (sosofo-append res
				(process-node-list c)
				($line-start$ indent line-numbers?
					      (+ linecount 1)))
		 (sosofo-append res (process-node-list c))))))))))

(define ($verbatim-display$ indent line-numbers?)
  (let* ((width-in-chars (if (attribute-string (normalize "width"))
			     (string->number (attribute-string (normalize "width")))
			     %verbatim-default-width%))
	 (fsize (lambda () (if (or (attribute-string (normalize "width"))
				   (not %verbatim-size-factor%))
			       (/ (/ (- %text-width% (inherited-start-indent))
				     width-in-chars) 
				  0.7)
			       (* (inherited-font-size) 
				  %verbatim-size-factor%))))
	 (vspace (if (INBLOCK?)
		     0pt
		     (if (INLIST?)
			 %para-sep% 
			 %block-sep%))))
    (make paragraph
      use: verbatim-style
      space-before: (if (and (string=? (gi (parent)) (normalize "entry"))
 			     (absolute-first-sibling?))
			0pt
			vspace)
      space-after:  (if (and (string=? (gi (parent)) (normalize "entry"))
 			     (absolute-last-sibling?))
			0pt
			vspace)
      font-size: (fsize)
      line-spacing: (* (fsize) %line-spacing-factor%)
      start-indent: (if (INBLOCK?)
			(inherited-start-indent)
			(+ %block-start-indent% (inherited-start-indent)))
      (if (or indent line-numbers?)
	  ($linespecific-line-by-line$ indent line-numbers?)
	  (process-children)))))

(element literallayout
  (if (equal? (attribute-string "class") (normalize "monospaced"))
      ($verbatim-display$
       %indent-literallayout-lines%
       (or %number-literallayout-lines%
	   (equal? (attribute-string (normalize "linenumbering"))
		   (normalize "numbered"))))
      ($linespecific-display$
       %indent-literallayout-lines%
       (or %number-literallayout-lines%
	   (equal? (attribute-string (normalize "linenumbering"))
		   (normalize "numbered"))))))

(element address
  ($linespecific-display$
   %indent-address-lines%
   (or %number-address-lines%
       (equal? (attribute-string (normalize "linenumbering"))
	       (normalize "numbered")))))

(element programlisting
  ($verbatim-display$
   %indent-programlisting-lines%
   (or %number-programlisting-lines%
       (equal? (attribute-string (normalize "linenumbering"))
	       (normalize "numbered")))))

(element screen
  ($verbatim-display$
   %indent-screen-lines%
   (or %number-screen-lines%
       (equal? (attribute-string (normalize "linenumbering"))
	       (normalize "numbered")))))

(element screenshot (process-children))
(element screeninfo (empty-sosofo))

