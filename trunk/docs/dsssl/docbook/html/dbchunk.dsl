;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

(define (chunk-element-list)
  (list (normalize "preface")
	(normalize "chapter")
	(normalize "appendix") 
	(normalize "article")
	(normalize "glossary")
	(normalize "bibliography")
	(normalize "index")
	(normalize "colophon")
	(normalize "setindex")
	(normalize "reference")
	(normalize "refentry")
	(normalize "part")
	(normalize "sect1") 
	(normalize "section") 
	(normalize "book") ;; just in case nothing else matches...
	(normalize "set")  ;; sets are definitely chunks...
	))

(define (chunk-skip-first-element-list)
  (list (normalize "sect1")
	(normalize "section")))

(define (chunk-section-depth)
  1)

(define (section-element-depth #!optional (section (current-node)))
  (if (node-list-empty? section)
      0
      (if (equal? (gi section) (normalize "section"))
	  (length (hierarchical-number-recursive
		   (normalize "section")
		   section))
	  (section-element-depth (parent section)))))

(define (is-first-element nd)
  (equal? (child-number nd) 1))

(define (combined-chunk? #!optional (nd (current-node)))
  (or
   ;; if it's a section and the parent element is also a section
   ;; and its depth is less than or equal to chunk-section-depth
   (and (equal? (gi nd) (normalize "section"))
	(not (node-list-empty? (parent nd)))
	(equal? (gi (parent nd)) (normalize "section"))
	(>= (section-element-depth nd) (chunk-section-depth)))
   ;; if it's the first skipped chunk in a chunk
   (and (not (node-list-empty? nd))
	(member (gi nd) (chunk-element-list))
	(is-first-element nd)
	(member (gi nd) (chunk-skip-first-element-list)))
   ;; or if it's a chunk in a partintro
   (and (member (gi nd) (chunk-element-list))
	(has-ancestor-member? nd (list (normalize "partintro"))))))

(define (chunk? #!optional (nd (current-node)))
  ;; 1. The (sgml-root-element) is always a chunk.
  ;; 2. If nochunks is #t or the dbhtml PI on the root element 
  ;;    specifies chunk='no', then the root element is the only
  ;;    chunk.
  ;; 3. Otherwise, elements in the chunk-element-list are chunks
  ;;    unless they're combined with their parent.
  ;; 4. Except for bibliographys, which are only chunks if they
  ;;    occur in book or article.
  ;; 5. And except for sections, which are only chunks if they
  ;;    are not too deep
  ;;
  (let* ((notchunk (or (and (equal? (gi nd) (normalize "bibliography"))
			    (not (or (equal? (gi (parent nd)) (normalize "book"))
				     (equal? (gi (parent nd)) (normalize "article")))))
		       (and (equal? (gi nd) (normalize "section"))
			    (equal? (gi (parent nd)) (normalize "section"))
			    (>= (section-element-depth nd)
				(chunk-section-depth)))))
	 (maybechunk (not notchunk)))
    (if (node-list=? nd (sgml-root-element))
	#t
	(if (or nochunks
		(equal? (dbhtml-value (sgml-root-element) "chunk") "no"))
	    #f
	    (if (member (gi nd) (chunk-element-list))
		(if (combined-chunk? nd)
		    #f
		    maybechunk)
		#f)))))

(define (html-prefix nd) 
  (let ((dbhtml-prefix (inherited-dbhtml-value nd "prefix")))
    (if dbhtml-prefix
	dbhtml-prefix
	%html-prefix%)))

(define (id-based-filename nd)
  (if (and %use-id-as-filename% 
	   (attribute-string (normalize "id") nd))
      (case-fold-down (attribute-string (normalize "id") nd))
      #f))

(define (book-html-base nd)
  (let ((number (number->string (all-element-number nd)))
	;(number (pad-string (number->string 3) 2 "0"))
	(prefix (html-prefix nd))
	(pibase (or
		 (inherited-dbhtml-value nd "basename")
		 (inherited-pi-value nd "html-basename")))
	(idbase (id-based-filename nd)))
    (if idbase
	(string-append (if prefix prefix "") idbase)
	(string-append (if prefix prefix "") 
		       (if pibase pibase "book") number))))

(define (division-html-base nd)
  (let* ((number (number->string (all-element-number nd)))
	 (prefix (html-prefix nd))
	 (pibase (or
		  (inherited-dbhtml-value nd "basename")
		  (inherited-pi-value nd "html-basename")))
	 (idbase (id-based-filename nd))
	 (base   (cond (pibase pibase)
		       (idbase idbase)
		       ((equal? (gi nd) (normalize "set"))          "s")
		       ((equal? (gi nd) (normalize "preface"))      "f")
		       ((equal? (gi nd) (normalize "chapter"))      "c")
		       ((equal? (gi nd) (normalize "article"))      "t")
		       ((equal? (gi nd) (normalize "appendix"))     "a")
		       ((equal? (gi nd) (normalize "part"))         "p")
		       ((equal? (gi nd) (normalize "reference"))    "r")
		       ((equal? (gi nd) (normalize "glossary"))     "g")
		       ((equal? (gi nd) (normalize "bibliography")) "b")
		       ((equal? (gi nd) (normalize "index"))        "i")
		       ((equal? (gi nd) (normalize "setindex"))     "n")
		       ((equal? (gi nd) (normalize "refentry"))     "r")
		       ;; "x" is section
		       (else "z"))))
    (if idbase
	(string-append (if prefix prefix "") idbase)
	(if pibase
	    (string-append (if prefix prefix "") pibase number)
	    (string-append (if prefix prefix "") base number)))))

(define (component-html-base nd)
  (division-html-base nd))

(define (section-html-base nd)
  ;; Now that I'm using all-element-number, there's no point in basing
  ;; it off the component-html-base at all...
  (let* ((number (number->string (all-element-number nd)))
	 (prefix (html-prefix nd))
	 (pibase (or
		  (inherited-dbhtml-value nd "basename")
		  (inherited-pi-value nd "html-basename")))
	 (idbase (id-based-filename nd))
	 (base   (if pibase
		     (string-append (if prefix prefix "") pibase)
		     (string-append (if prefix prefix "") "x"))))
    (if idbase
	(string-append (if prefix prefix "") idbase)
	(if (chunk? nd)
	    (string-append base number)
	    base))))

(define (element-html-base nd)
  (let* ((number (number->string (all-element-number nd)))
	 (prefix (html-prefix nd))
	 (pibase (or
		  (inherited-dbhtml-value nd "basename")
		  (inherited-pi-value nd "html-basename")))
	 (idbase (id-based-filename nd))
	 (base   (if pibase
		     (string-append (if prefix prefix "") pibase)
		     (string-append (if prefix prefix "") 
				    (case-fold-down (gi nd))))))
    (if idbase
	(string-append (if prefix prefix "") idbase)
	(string-append base number))))

;; Returns the filename of the html file that contains elemnode, without
;; any leading path information
(define (html-base-filename #!optional (input_nd (current-node)))
  (let* ((nd       (chunk-parent input_nd))
	 (base     (cond ((member (gi nd) (book-element-list))
			  (book-html-base nd))
			 ((member (gi nd) (division-element-list))
			  (division-html-base nd))
			 ((member (gi nd) (component-element-list))
			  (component-html-base nd))
			 ((member (gi nd) (section-element-list))
			  (section-html-base nd))
			 (else (element-html-base input_nd))))
	 ;; If this chunk-level element isn't a chunk, get the pifile from
	 ;; the parent element.
	 (pifile   (if (chunk? nd)
		       (or
			(dbhtml-value nd "filename")
			(pi-value nd "html-filename"))
		       (or
			(dbhtml-value (parent nd) "filename")
			(pi-value (parent nd) "html-filename"))))
	 (language (if %html-use-lang-in-filename%
		       (if (inherited-attribute-string (normalize "lang") nd)
			   (inherited-attribute-string (normalize "lang") nd)
			   %default-language%)
		       ""))
	 (ext      (if %html-use-lang-in-filename%
		       (string-append "." language %html-ext%)
		       %html-ext%)))
    (if (and %root-filename% (node-list=? (sgml-root-element) nd))
	(string-append %root-filename% ext)
	(if pifile 
	    pifile
	    (string-append base ext)))))

(define (root-rel-path filename #!optional (node (current-node)))
  ;; Return the filename relative to the root path
  (string-append (copy-string "../" (directory-depth (html-file node)))
		 filename))

;; Returns the filename of the html file that contains elemnode
;;
(define (html-file #!optional (input_nd (current-node)))
  (let* ((cp-nd         (chunk-parent input_nd))
	 ;; If the sgml-root-element is at a level below the chunking
	 ;; level, then cp-nd will return an empty-node-list. In this
	 ;; case, we want to return the root-element.
	 (nd            (if (node-list-empty? cp-nd)
			    (sgml-root-element)
			    cp-nd))
	 (base-filename (html-base-filename nd))
	 (pidir (or
		 (inherited-dbhtml-value nd "dir")
     		 (inherited-pi-value nd "html-dir"))))
    (if (and %root-filename% (node-list=? (sgml-root-element) nd))
	base-filename
	(if pidir
	    (string-append pidir "/" base-filename)
	    base-filename))))

(define (href-to target)
  ;; Return the HTML HREF for the given node.  If nochunks is true, just
  ;; return the fragment identifier.
  (let* ((id       (element-id target))
	 (curdepth (directory-depth (html-file (current-node))))
	 (entfile  (html-file target))
	 (fragid   (if (chunk? target)
		     ""
		     (string-append "#" id))))
    (if nochunks
	fragid
	(string-append (copy-string "../" curdepth) entfile fragid))))

(define (html-entity-file htmlfilename)
  ;; Returns the filename that should be used for _writing_ htmlfilename.
  ;; This may differ from the filename used in referencing it.  (The point
  ;; is that you can force the stylesheets to write the chunked files
  ;; somewhere else, if you want.)
  (let* ((pi-outputdir (dbhtml-value (sgml-root-element) "output-dir"))
	 (outputdir    (if pi-outputdir
			   pi-outputdir
			   %output-dir%)))
    (if (and use-output-dir outputdir)
	(string-append outputdir "/" htmlfilename)
	htmlfilename)))
  
;; Split node list nl at nd; return '(nodes-prev-to-nd nodes-following-nd)
;; Note that nd does not appear in either return list.
(define (split-node-list nd nodelist)
  (let loop ((prev (empty-node-list)) 
	     (nl nodelist))
    (if (node-list-empty? nl)
	(list prev (empty-node-list))
	(if (node-list=? (node-list-first nl) nd)
	    (list prev (node-list-rest nl))
	    (loop (node-list prev (node-list-first nl))
		  (node-list-rest nl))))))

(define (navigate-to? nd)
  #t)

(define (chunk-parent #!optional (nd (current-node)))
  (let ((cp (let loop ((p (chunk-level-parent nd)))
	      (if (or (node-list-empty? p) (chunk? p))
		  p
		  (chunk-parent (parent p))))))
    cp))
;    (if (node-list-empty? cp)
;	;; if there's no chunk-parent, return the root node
;	(sgml-root-element)
;	;; otherwise, return the parent that we found
;	cp)))

(define (chunk-level-parent #!optional (nd (current-node)))
  (ancestor-member nd (chunk-element-list)))

(define (chunk-children #!optional (nd (current-node)))
  (node-list-filter-by-gi (children nd) (chunk-element-list)))

(define (ifollow-by-gi nd gilist)
  (let loop ((next (ifollow nd)))
    (if (node-list-empty? next)
	(empty-node-list)
	(if (member (gi next) gilist)
	    next
	    (loop (ifollow next))))))

(define (ipreced-by-gi nd gilist)
  (let loop ((prev (ipreced nd)))
    (if (node-list-empty? prev)
	(empty-node-list)
	(if (member (gi prev) gilist)
	    prev
	    (loop (ipreced prev))))))

(define (last-chunk-element nd)
  (let ((clc (node-list-filter-by-gi (children nd) (chunk-element-list))))
    (if (node-list-empty? clc)
	nd
	(last-chunk-element (node-list-last clc)))))

(define (next-chunk-skip-children #!optional (elem (current-node)))
  (let* ((nd  (chunk-level-parent elem))
	 (psl (node-list-filter-by-gi (children (parent nd)) 
				      (chunk-element-list)))
	 (nextlist (car (cdr (split-node-list nd psl)))))
    (if (node-list-empty? nextlist)
	(if (node-list-empty? (parent nd))
	    (empty-node-list)
	    (next-chunk-skip-children (parent nd)))
	(node-list-first nextlist))))

(define (next-chunk-with-children #!optional (elem (current-node)))
  (let* ((nd  (chunk-level-parent elem))
	 (clc (chunk-children nd))
	 (ns  (ifollow-by-gi nd (chunk-element-list))))
    (if (node-list-empty? clc)
	(if (node-list-empty? ns)
	    (next-chunk-skip-children (parent nd))
	    (node-list-first ns))
	;; If the first of the chunk-children (clc) of this element
	;; isn't its own chunk, skip over it, otherwise it's next.
	(if (chunk? (node-list-first clc))
	    (node-list-first clc)
	    (next-chunk-with-children (node-list-first clc))))))
;;	    (if (> (node-list-length clc) 1)
;;		(node-list-first (node-list-rest clc))
;;		(next-chunk-skip-children nd))))))

(define (abs-prev-chunk #!optional (elem (current-node)))
  (let* ((nd  (chunk-parent elem))
	 (pse (ipreced-by-gi nd (chunk-element-list)))
	 (ps  (chunk-parent pse)))
    (if (node-list-empty? ps)
	(parent nd)
	(last-chunk-element ps))))

(define (prev-chunk-element #!optional (elem (current-node)))
  (let* ((nd   (chunk-parent elem))
	 (prev (chunk-parent (abs-prev-chunk nd))))
    ;; There's a special case here.  abs-prev-chunk always returns the last
    ;; chunk element of the preceding element if we walk up the tree.  This
    ;; assures that the last section of the preceding chapter is the "prev"
    ;; element of the current chapter.
    ;;
    ;; However, if chunk-skip-first-element is in use, then abs-prev-chunk
    ;; gets fooled when it tries to find the element that precedes the
    ;; second child element that's in chunk-skip-first-element list.
    ;;
    ;; For example, if SECT1 is in chunk-skip-first-element then the
    ;; chunk that precedes the second SECT1 in a CHAPTER is the CHAPTER
    ;; (not the first SECT1 because the first SECT1 is "skipped", 
    ;; it's in the CHAPTER chunk).  Confused yet?
    ;;
    ;; Ok, now unfortunately, what abs-prev-chunk returns is the last child
    ;; of the CHAPTER, so instead of going from the second SECT1 to the
    ;; CHAPTER, we go from the second SECT1 to the last SECT1 of the CHAPTER.
    ;;
    ;; I can't think of a good way to handle this except to test for it
    ;; right up front.  I wonder if all this skip stuff was really worth it?
    ;;
    (if (and (member (gi elem) (chunk-skip-first-element-list))
	     (equal? (child-number elem) 2))
	;; this is the second child, the prev node is the parent.
	(parent elem)
	;; otherwise, do the "normal" thing to find it:
	(if (node-list-empty? prev)
	    prev
	    (if (combined-chunk? prev)
		(parent prev)
		(if (and (chunk? nd)
			 (chunk? prev)
			 (navigate-to? prev))
		    prev
		    (prev-chunk-element prev)))))))

(define (abs-prev-peer-chunk-element #!optional (elem (current-node)))
  ;; Returns the previous element that is a sibling or parent of the
  ;; current element.  Absolute in this case refers to the fact that
  ;; it returns the immediate predecessor without regard for whether or
  ;; not it is a chunk.
  (let* ((psibling (if (node-list-empty? (preced elem))
		       (empty-node-list)
		       (node-list-last (preced elem)))))
    (if (node-list-empty? psibling)
	(parent elem)
	psibling)))

(define (prev-peer-chunk-element #!optional (elem (current-node)))
  (let loop ((nd (chunk-level-parent elem)))
    (if (node-list-empty? nd)
	(empty-node-list)
	(if (and (chunk? (abs-prev-peer-chunk-element nd))
		 (navigate-to? (abs-prev-peer-chunk-element nd)))
	    (abs-prev-peer-chunk-element nd)
	    (loop (abs-prev-peer-chunk-element nd))))))

(define (prev-major-component-chunk-element #!optional (elem (current-node)) (in-chain #f))
  ;; Return the prev major component of the document that is a sibling (or
  ;; ancestor) of the starting element. This is essentially 'prev-sibling' 
  ;; but skips over things that aren't chunks.
  (if (or (navigate-to? elem) in-chain)
      (if (member (gi elem) (major-component-element-list))
	  (if (node-list-empty? (node-list-last-element (preced elem)))
	      (prev-chunk-element elem)
	      (let ((nd (node-list-last-element (preced elem))))
		(if (navigate-to? nd)
		    nd
		    (prev-major-component-chunk-element nd #t))))
	  (ancestor-member elem (major-component-element-list)))
      (empty-node-list)))

(define (abs-next-chunk #!optional (elem (current-node)) (children-ok? #t))
  (let* ((nd  (chunk-level-parent elem))
	 (clc (if children-ok? (chunk-children nd) (empty-node-list)))
	 (ns  (ifollow-by-gi nd (chunk-element-list))))
    (if (node-list-empty? clc)
	(if (node-list-empty? ns)
	    (if (node-list-empty? (parent nd))
		(empty-node-list)
		(abs-next-chunk (parent nd) #f))
	    (node-list-first ns))
	(node-list-first clc))))

(define (next-chunk-element #!optional (elem (current-node)))
  (let ((next (abs-next-chunk elem)))
    (if (node-list-empty? next)
	(empty-node-list)
	(if (chunk? next)
	    (if (navigate-to? next)
		next
		(next-chunk-element next))
	    (next-chunk-element next)))))

(define (abs-next-peer-chunk-element #!optional (elem (current-node)))
  (let* ((fsibling (if (node-list-empty? (follow elem))
		       (empty-node-list)
		       (node-list-first (follow elem)))))
    (if (node-list-empty? fsibling)
	(if (node-list-empty? (parent elem))
	    (empty-node-list)
	    (abs-next-peer-chunk-element (parent elem)))
	fsibling)))

(define (next-peer-chunk-element #!optional (elem (current-node)))
  (let loop ((nd (chunk-level-parent elem)))
    (if (node-list-empty? nd)
	(empty-node-list)
	(if (and (chunk? (abs-next-peer-chunk-element nd))
		 (navigate-to? (abs-next-peer-chunk-element nd)))
	    (abs-next-peer-chunk-element nd)
	    (loop (abs-next-peer-chunk-element nd))))))

(define (next-major-component-chunk-element #!optional (elem (current-node)) (in-chain #f))
  ;; Return the next major component of the document that is not a descendant
  ;; of the starting element.  This is essentially 'next-sibling' but skips
  ;; over things that aren't chunks.
  (if (or (navigate-to? elem) in-chain)
      (if (member (gi elem) (major-component-element-list))
	  (if (node-list-empty? (node-list-first-element (follow elem)))
	      (next-major-component-chunk-element (parent elem))
	      (let ((nd (node-list-first-element (follow elem))))
		(if (navigate-to? nd)
		    nd
		    (next-major-component-chunk-element nd #t))))
	  (ancestor-member elem (major-component-element-list)))
      (empty-node-list)))

;; EOF dbchunk.dsl