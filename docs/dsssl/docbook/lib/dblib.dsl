<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">

<style-sheet>
<style-specification>
<style-specification-body>

;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;
;; This file contains a general library of DSSSL functions.
;;

;; If **ANY** change is made to this file, you _MUST_ alter the
;; following definition:

;; REFERENCE Library Version

(define %library-version%
  ;; REFENTRY version
  ;; PURP Defines the library version string
  ;; DESC
  ;; Defines the library version string.
  ;; /DESC
  ;; /REFENTRY
  "Modular DocBook Stylesheet Library")

;; === Book intro, for dsl2man ==========================================

<![CDATA[
;; DOCINFO
;; <title>DSSSL Library</title>
;; <subtitle>Part of the Modular DocBook Stylesheet distribution</subtitle>
;; <author><firstname>Norman</firstname><surname>Walsh</surname>
;; </author>
;; <edition>$Revision$</edition>
;; <copyright><year>1997</year><year>1998</year><year>1999</year>
;; <holder>Norman Walsh</holder></copyright>
;; <legalnotice>
;; <para>
;; This software may be distributed under the same terms as Jade:
;; </para>
;; <para>
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the &ldquo;Software&rdquo;), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;; </para>
;; <para>
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; </para>
;; <para>
;; Except as contained in this notice, the names of individuals
;; credited with contribution to this software shall not be used in
;; advertising or otherwise to promote the sale, use or other
;; dealings in this Software without prior written authorization
;; from the individuals in question.
;; </para>
;; <para>
;; Any stylesheet derived from this Software that is publically
;; distributed will be identified with a different name and the
;; version strings in any derived Software will be changed so that
;; no possibility of confusion between the derived package and this
;; Software will exist.
;; </para>
;; </legalnotice>
;; <legalnotice>
;; <para>
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL NORMAN WALSH OR ANY OTHER
;; CONTRIBUTOR BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;; </para>
;; </legalnotice>
;; <legalnotice>
;; <para>Please direct all questions, bug reports, or suggestions for changes
;; to Norman Walsh, &lt;<literal>ndw@nwalsh.com</literal>&gt;.
;; </para>
;; <para>
;; See <ulink url="http://nwalsh.com/docbook/dsssl/">http://nwalsh.com/docbook/dsssl/</ulink> for more information.</para>
;; </legalnotice>
;; /DOCINFO
]]>

;; === Some additional units ============================================

(define-unit pi (/ 1in 6))
(define-unit pt (/ 1in 72))
(define-unit px (/ 1in 96))

;; REFERENCE ISO/IEC 10179

(define (node-list-reduce nl proc init)
  ;; REFENTRY node-list-reduce
  ;; PURP Implements node-list-reduce as per ISO/IEC 10179:1996
  ;; DESC
  ;; Implements 'node-list-reduce' as per ISO/IEC 10179:1996
  ;; /DESC
  ;; AUTHOR From ISO/IEC 10179:1996
  ;; /REFENTRY
  (if (node-list-empty? nl)
      init
      (node-list-reduce (node-list-rest nl)
                        proc
                        (proc init (node-list-first nl)))))

(define (node-list-last nl)
  ;; REFENTRY node-list-last
  ;; PURP Implements node-list-last as per ISO/IEC 10179:1996
  ;; DESC
  ;; Implements 'node-list-last' as per ISO/IEC 10179:1996
  ;; /DESC
  ;; AUTHOR From ISO/IEC 10179:1996
  ;; /REFENTRY
  (node-list-ref nl
		 (- (node-list-length nl) 1)))

(define (node-list-first-element nodelist)
  ;; REFENTRY node-list-first-element
  ;; PURP Return the first element node in a node list
  ;; DESC
  ;; This function returns the first node in a node list which is
  ;; an element (as opposed to a PI or anything else that might appear
  ;; in a node list).
  ;; /DESC
  ;; /REFENTRY
  (let loop ((nl nodelist))
    (if (node-list-empty? nl)
	(empty-node-list)
	(if (gi (node-list-first nl))
	    (node-list-first nl)
	    (loop (node-list-rest nl))))))

(define (node-list-last-element nodelist)
  ;; REFENTRY node-list-last-element
  ;; PURP Return the last element node in a node list
  ;; DESC
  ;; This function returns the last node in a node list which is
  ;; an element (as opposed to a PI or anything else that might appear
  ;; in a node list).
  ;; /DESC
  ;; /REFENTRY
  (let loop ((el (empty-node-list)) (nl nodelist))
    (if (node-list-empty? nl)
	el
	(if (gi (node-list-first nl))
	    (loop (node-list-first nl) (node-list-rest nl))
	    (loop el (node-list-rest nl))))))

(define (ipreced nl)
  ;; REFENTRY ipreced
  ;; PURP Implements ipreced as per ISO/IEC 10179:1996
  ;; DESC
  ;; Implements 'ipreced' as per ISO/IEC 10179:1996
  ;; /DESC
  ;; AUTHOR From ISO/IEC 10179:1996
  ;; /REFENTRY
  (node-list-map (lambda (snl)
		 (let loop ((prev (empty-node-list))
			    (rest (siblings snl)))
		   (cond ((node-list-empty? rest)
			  (empty-node-list))
			 ((node-list=? (node-list-first rest) snl)
			  prev)
			 (else
			  (loop (node-list-first rest)
				(node-list-rest rest))))))
		 nl))


(define (ifollow nl)
  ;; REFENTRY ifollow
  ;; PURP Implements ifollow as per ISO/IEC 10179:1996
  ;; DESC
  ;; Implements 'ifollow' as per ISO/IEC 10179:1996
  ;; /DESC
  ;; AUTHOR From ISO/IEC 10179:1996
  ;; /REFENTRY
  (node-list-map (lambda (snl)
		   (let loop ((rest (siblings snl)))
		     (cond ((node-list-empty? rest)
			    (empty-node-list))
			   ((node-list=? (node-list-first rest) snl)
			    (node-list-first (node-list-rest rest)))
			   (else
			    (loop (node-list-rest rest))))))
		 nl))

(define (siblings snl)
  ;; REFENTRY siblings
  ;; PURP Implements siblings as per ISO/IEC 10179:1996
  ;; DESC
  ;; Implements 'siblings' as per ISO/IEC 10179:1996
  ;; /DESC
  ;; AUTHOR From ISO/IEC 10179:1996
  ;; /REFENTRY
  (children (parent snl)))

(define (string->list str)
  ;; REFENTRY string-2-list
  ;; PURP Converts a string into a list of characters.
  ;; DESC
  ;; Implements 'string->list' as per ISO/IEC 10179:1996
  ;; (clause 8.5.9.9).
  ;; /DESC
  ;; AUTHOR David Megginson
  ;; EMAIL dmeggins@uottawa.ca
  ;; /REFENTRY
  (let loop ((chars '())
	     (k (- (string-length str) 1)))
    (if (< k 0)
	chars
	(loop (cons (string-ref str k) chars) (- k 1)))))

(define (list->string chars)
  ;; REFENTRY list-2-string
  ;; PURP Converts a list of characters into a string
  ;; DESC
  ;; Implements 'list->string' as per ISO/IEC 10179:1996
  ;; (clause 8.5.9.9).
  ;; /DESC
  ;; AUTHOR David Megginson
  ;; EMAIL dmeggins@uottawa.ca
  ;; /REFENTRY
  (let loop ((cl chars)
	     (str ""))
    (if (null? cl)
	str
	(loop (cdr cl)
	      (string-append str (string (car cl)))))))

;; ======================================================================

(define (map f #!rest xs)
  ;; REFENTRY map
  ;; PURP Implements map
  ;; DESC
  ;; Implements map
  ;; /DESC
  ;; AUTHOR From Mulberry Tech. site (need better attribution)
  ;; /REFENTRY
  (let ((map1 (lambda (f xs)           ; bootstrap version for unary F
		(let loop ((xs xs))
		  (if (null? xs)
		      '()
		      (cons (f (car xs))
			    (loop (cdr xs))))))))
    (cond ((null? xs)
	   '())
	  ((null? (cdr xs))
	   (map1 f (car xs)))
	  (else
	   (let loop ((xs xs))
	     (if (null? (car xs))
		 '()
		 (cons (apply f (map1 car xs))
		       (loop (map1 cdr xs)))))))))

(define (absolute-child-number #!optional (nd (current-node)))
  ;; REFENTRY absolute-child-number
  ;; PURP Returns the absolute child number of the specified node
  ;; DESC
  ;; Returns the child number, regardless of gi, of 'snl' within its
  ;; parent.
  ;;
  ;; Isn't there a better way to get this?
  ;; ARGS
  ;; ARG snl
  ;; The node (singleton node list) whose child number is desired.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (+ (node-list-length (preced nd)) 1))

;; REFERENCE Debug

(define (my-debug x #!optional return-value)
  ;; REFENTRY my-debug
  ;; PURP A debugging function more helpful than (debug)
  ;; DESC
  ;; A version of debug that tries to print information more helpful
  ;; than "unknown object ...".  Will need extending for any further
  ;; types added to Jade which don't have useful print methods.
  ;; (Should yield more information extracted from each type.)
  ;; ARGS
  ;; ARG x
  ;; The object about which debugging information is desired.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; AUTHOR Tony Graham
  ;; /REFENTRY
  (let ((msg (debug (cond ((node-list? x)
			   (if (node-list-empty? x)
			       (list 'empty-node-list x)
			       (list (if (named-node-list? x)
					 'named-node-list
					 'node-list)
				     (node-list-length x) x)))
			  ((sosofo? x)
			   (list 'sosofo x))
			  ((procedure? x)
			   (list 'procedure x))
			  ((style? x)
			   (list 'style x))
			  ((address? x)
			   (list 'address x))
			  ((color? x)
			   (list 'color x))
			  ((color-space? x)
			   (list 'color-space x))
			  ((display-space? x)
			   (list 'display-space x))
			  ((inline-space? x)
			   (list 'inline-space x))
			  ((glyph-id? x)
			   (list 'glyph-id x))
			  ((glyph-subst-table? x)
			   (list 'glyph-subst-table x))
			  (else x)))))
    x))

;; REFERENCE Miscellaneous

(define (string-with-space string #!optional (space " ")) 
  ;; REFENTRY string-with-space
  ;; PURP Returns string with a space appended or the empty string
  ;; DESC
  ;; If 'string' is not the empty string, returns 'string' with a
  ;; 'space' appended.  If 'string' is empty, or is not a '(string?)',
  ;; returns 'string' unmodified.
  ;; ARGS
  ;; ARG 'string'
  ;; The string onto which a space should be appended.
  ;; /ARG
  ;; ARG 'space' o
  ;; If specified, the space to append.  Defaults to a single space.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (if (string? string)
      (if (equal? string "")
	  string
	  (string-append string space))
      string))

;; ======================================================================

(define (split str #!optional (whitespace '(#\space)))
  ;; REFENTRY split
  ;; PURP Splits string at whitespace and returns the resulting list of tokens
  ;; DESC
  ;; Given a string containing delimited tokens, return a list
  ;; of the tokens in string form.
  ;; ARGS
  ;; ARG 'str'
  ;; The string to split.
  ;; /ARG
  ;; ARG 'whitespace' o
  ;; A list of characters that should
  ;; be treated as whitespace.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; AUTHOR David Megginson
  ;; EMAIL dmeggins@uottawa.ca
  ;; /REFENTRY
  (let loop ((characters (string->list str)) ; Top-level recursive loop.
	     (current-word '())
	     (tokens '()))

    ; If there are no characters left,
    ; then we're done!
    (cond ((null? characters)
	   ; Is there a token in progress?
	   (if (null? current-word)
	       (reverse tokens)
	       (reverse (cons (list->string (reverse current-word))
			      tokens))))
	  ; If there are characters left,
	  ; then keep going.
	  (#t
	   (let ((c (car characters))
		 (rest (cdr characters)))
	     ; Are we reading a space?
	     (cond ((member c whitespace)
		    (if (null? current-word)
			(loop rest '() tokens)
			(loop rest
			      '()
			      (cons (list->string (reverse current-word))
				    tokens))))
		   ; We are reading a non-space
		   (#t
		    (loop rest (cons c current-word) tokens))))))))

;; ======================================================================

(define (strip str #!optional (stripchars '(#\space #\&#RE #\U-0009)))
  ;; REFENTRY strip
  ;; PURP Strip leading and trailing characters off of a string
  ;; DESC
  ;; Strips leading and trailing characters in the 'stripchars' list
  ;; off of a string and returns the stripped string.
  ;; ARGS
  ;; ARG 'str'
  ;; The string to strip
  ;; /ARG
  ;; ARG 'stripchars' o
  ;; A list of characters that should
  ;; be stripped.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (let* ((startpos (let loop ((count 0))
		     (if (>= count (string-length str))
			 (string-length str)
			 (if (member (string-ref str count) stripchars)
			     (loop (+ count 1))
			     count))))
	 (tailstr  (substring str startpos (string-length str)))
	 (endpos   (let loop ((count (- (string-length tailstr) 1)))
		     (if (< count 1)
			 0
			 (if (member (string-ref tailstr count) stripchars)
			     (loop (- count 1))
			     count)))))
    (if (or (< endpos 0)
	    (string=? tailstr ""))
	""
	(substring tailstr 0 (+ endpos 1)))))

;; ======================================================================

(define (join slist #!optional (space " "))
  ;; REFENTRY join
  ;; PURP Joins a list of strings together
  ;; DESC
  ;; Given a list of strings and a space string, returns the string
  ;; that results from joining all the strings in the list together,
  ;; separated by space.
  ;; ARGS
  ;; ARG 'slist'
  ;; The list of strings.
  ;; /ARG
  ;; ARG 'space' o
  ;; The string to place between each member of the list.  Defaults to
  ;; a single space.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; AUTHOR David Carlisle
  ;; /REFENTRY
  (letrec ((loop (lambda (l result)
		   (if (null? l) 
		       result
		       (loop (cdr l) (cons space (cons (car l) result)))))))
    (if (null? slist)
	""
	(apply string-append (cons (car slist) 
				   (loop (reverse (cdr slist)) '() ))))))

;; ======================================================================

(define (pad-string string length padchar)
  ;; REFENTRY pad-string
  ;; PURP Pads a string, in front, to the specified length
  ;; DESC
  ;; Returns 'string', padded in front with 'padchar' to at least 'length'
  ;; Returns 'string' unmodified if 'string' is not a '(string?)',
  ;; 'padchar' is not a '(string?)', 'padchar' is the empty string, or if
  ;; 'string' is already greater than or equal to 'length' in length.
  ;; ARGS
  ;; ARG 'string'
  ;; The string to pad.
  ;; /ARG
  ;; ARG 'length'
  ;; The desired length.
  ;; /ARG
  ;; ARG 'padchar'
  ;; The character (string, actually) to use as padding.  If 'padchar' is
  ;; longer than 1 character, the resulting string may be longer than
  ;; 'length' when returned.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (if (and (string? string) 
	   (string? padchar) 
	   (> (string-length padchar) 0))
      (let loop ((s string) (count (- length (string-length string))))
	(if (<= count 0)
	    s
	    (loop (string-append padchar s) 
		  (- count (string-length padchar)))))
      string))

;; ======================================================================

(define (match-split string target)
  ;; REFENTRY match-split
  ;; PURP Splits string at target and returns the resulting list of tokens
  ;; DESC
  ;; Splits string at every occurance of target and returns the result
  ;; as a list.  Note that 'match-split' returns the occurances of 'target'
  ;; in the list of tokens.
  ;; ARGS
  ;; ARG 'string'
  ;; The string to split.
  ;; /ARG
  ;; ARG 'target'
  ;; The string which is a delimiter between tokens
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; EXAMPLE
  ;; '"this is a test"' split at '"is"' returns
  ;; '("th" "is" " " "is" " a test")'
  ;; /EXAMPLE
  ;; /REFENTRY
  (if (string? string)
      (let loop ((result '()) (current "") (rest string))
	(if (< (string-length rest) (string-length target))
	    (append result (if (equal? (string-append current rest) "")
			       '()
			       (list (string-append current rest))))
	    (if (equal? target (substring rest 0 (string-length target)))
		(loop (append result 
			      (if (equal? current "")
				  '()
				  (list current))
			      (list target))
		      ""
		      (substring rest (string-length target) 
				 (string-length rest)))
		(loop result
		      (string-append current (substring rest 0 1))
		      (substring rest 1 (string-length rest))))))
      (list string)))

(define (match-split-string-list string-list target)
  ;; REFENTRY match-split-string-list
  ;; PURP Splits each string in a list of strings and returns the concatenated result list
  ;; DESC
  ;; Splits each string in 'string-list' at 'target' with '(match-split)',
  ;; concatenates the results, and returns a single list of tokens.
  ;; ARGS
  ;; ARG string-list
  ;; The list of strings to split.
  ;; /ARG
  ;; ARG target
  ;; The string which is a delimiter between tokens.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (let loop ((result '()) (sl string-list))
    (if (null? sl)
	result
	(loop (append result (match-split (car sl) target))
	      (cdr sl)))))

(define (match-split-list string target-list)
  ;; REFENTRY match-split-list
  ;; PURP Splits a string at a list of targets and returns the resulting list of tokens
  ;; DESC
  ;; Splits 'string' at every target in 'target-list' with '(match-split)',
  ;; returning the whole collection of tokens as a list.
  ;; ARGS
  ;; ARG string
  ;; The string to split.
  ;; /ARG
  ;; ARG target-list
  ;; A list of target strings which are the delimters between tokens.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (let loop ((result (list string)) (tlist target-list))
    (if (null? tlist)
	result
	(loop (match-split-string-list result (car tlist))
	      (cdr tlist)))))

;; ======================================================================

(define (assoc-objs alist)
  ;; REFENTRY assoc-objs
  ;; PURP Returns a list of the objects in an associative list
  ;; DESC
  ;; Returns a list of the objects in an associative list.
  ;; ARGS
  ;; ARG alist
  ;; The associative list. An associative list is a list of lists
  ;; where each interior list is a pair of elements.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; EXAMPLE
  ;; '(assoc-objs (("a" "b") ("c" "d")))' returns '("a" "c")'
  ;; /EXAMPLE
  ;; /REFENTRY
  (let loop ((result '()) (al alist))
    (if (null? al)
	result
	(loop (append result (list (car (car al)))) (cdr al)))))

(define (assoc obj alist)
  ;; REFENTRY assoc
  ;; PURP Returns the association of an object in an associative list
  ;; DESC
  ;; Given an associative list, returns the pair that has 'obj' as a 'car'
  ;; or '#f' if no such pair exists.
  ;; ARGS
  ;; ARG obj
  ;; The associative key to locate.
  ;; /ARG
  ;; ARG alist
  ;; The associative list.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; EXAMPLE
  ;; '(assoc "a" (("a" "b") ("c" "d")))' returns  '("a" "b")'
  ;; /EXAMPLE
  ;; /REFENTRY
  (let loop ((al alist))
    (if (null? al)
	#f
	(if (equal? obj (car (car al)))
	    (car al)
	    (loop (cdr al))))))

(define (match-substitute-sosofo string assoc-list)
  ;; REFENTRY match-substitute-sosofo
  ;; PURP Return matching sosofo from associative list
  ;; DESC
  ;; Given a string and an associative list of strings and sosofos,
  ;; return the sosofo of the matching string, or return the literal
  ;; string as a sosofo.
  ;;
  ;; (This function is used for a particular task in the DocBook stylesheets.
  ;; It may not be particularly general, but it's in 'dblib.dsl' because
  ;; there is nothing DTD-specific about it.)
  ;; /DESC
  ;; /REFENTRY
  (if (assoc string assoc-list)
      (car (cdr (assoc string assoc-list)))
      (literal string)))

(define (string-list-sosofo string-list assoc-list)
  ;; REFENTRY string-list-sosofo
  ;; PURP Build sosofo from a list of strings and an associative list
  ;; DESC
  ;; Take a list of strings and an associative list that maps strings
  ;; to sosofos and return an appended sosofo.
  ;;
  ;; (This function is used for a particular task in the DocBook stylesheets.
  ;; It may not be particularly general, but it's in 'dblib.dsl' because
  ;; there is nothing DTD-specific about it.)
  ;; /DESC
  ;; EXAMPLE
  ;; Given the string list '("what is " "1" " " "+" " " "1")'
  ;; and the associative list 
  ;; '(("1" (literal "one")) ("2" (literal "two")) ("+" (literal "plus")))',
  ;; '(string-list-sosofo)' returns the sequence of sosofos
  ;; equivalent to '(literal "what is one plus one")'.
  ;; /EXAMPLE
  ;; /REFENTRY
  (if (null? string-list)
      (empty-sosofo)
      (sosofo-append (match-substitute-sosofo (car string-list) assoc-list)
		     (string-list-sosofo (cdr string-list) assoc-list))))

;; ======================================================================

(define (repl-substring? string target pos)
  ;; REFENTRY repl-substring-p
  ;; PURP Returns true if the specified substring can be replaced
  ;; DESC
  ;; Returns '#t' if 'target' occurs at 'pos' in 'string'.
  ;; /DESC
  ;; /REFENTRY
  (let* ((could-match (<= (+ pos (string-length target)) 
			 (string-length string)))
	 (match (if could-match 
		    (substring string pos (+ pos (string-length target))) "")))
    (and could-match (string=? match target))))

(define (repl-substring string target repl pos)
  ;; REFENTRY repl-substring
  ;; PURP Replace substring in a string
  ;; DESC
  ;; Replaces 'target' with 'repl' in 'string' at 'pos'.
  ;; /DESC
  ;; /REFENTRY
  (let ((matches (repl-substring? string target pos)))
    (if matches
	(string-append
	 (substring string 0 pos)
	 repl
	 (substring string 
		    (+ pos (string-length target)) 
		    (string-length string)))
	string)))

(define (repl-substring-list? string replace-list pos)
  ;; REFENTRY repl-substring-list-p
  ;; PURP Perform repl-substring? with a list of target/replacement pairs
  ;; DESC
  ;; Returns '#t' if any target in 'replace-list' occurs at 'pos' in 'string'.
  ;; ARGS
  ;; ARG 'string'
  ;; The string in which replacement should be tested.
  ;; /ARG
  ;; ARG 'replace-list'
  ;; A list of target/replacement pairs.  This list is just a list of
  ;; strings, treated as pairs.  For example, '("was" "x" "is" "y")'.
  ;; In this example, 'was' may be replaced by 'x' and 'is' may be
  ;; replaced by 'y'.
  ;; /ARG
  ;; ARG 'pos'
  ;; The location within 'string' where the test will occur.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; EXAMPLE
  ;; '(repl-substring-list? "this is it" ("was" "x" "is" "y") 2)'
  ;; returns '#t': "is" could be replaced by "y".
  ;; /EXAMPLE
  ;; /REFENTRY
  (let loop ((list replace-list))
    (let ((target (car list))
	  (repl   (car (cdr list)))
	  (rest   (cdr (cdr list))))
      (if (repl-substring? string target pos)
	  #t
	  (if (null? rest)
	      #f
	      (loop rest))))))

(define (repl-substring-list-target string replace-list pos)
  ;; REFENTRY repl-substring-list-target
  ;; PURP Return the target that matches in a string
  ;; DESC
  ;; Returns the target in 'replace-list' that matches in 'string' at 'pos'
  ;; See also 'repl-substring-list?'.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((list replace-list))
    (let ((target (car list))
	  (repl   (car (cdr list)))
	  (rest   (cdr (cdr list))))
      (if (repl-substring? string target pos)
	  target
	  (if (null? rest)
	      #f
	      (loop rest))))))

(define (repl-substring-list-repl string replace-list pos)
  ;; REFENTRY repl-substring-list-repl
  ;; PURP Return the replacement that would be used in the string
  ;; DESC
  ;; Returns the replacement in 'replace-list' that would be used for the
  ;; target that matches in 'string' at 'pos'
  ;; See also 'repl-substring-list?'.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((list replace-list))
    (let ((target (car list))
	  (repl   (car (cdr list)))
	  (rest   (cdr (cdr list))))
      (if (repl-substring? string target pos)
	  repl
	  (if (null? rest)
	      #f
	      (loop rest))))))

(define (repl-substring-list string replace-list pos)
  ;; REFENTRY repl-substring-list
  ;; PURP Replace the first target in the replacement list that matches
  ;; DESC
  ;; Replaces the first target in 'replace-list' that matches in 'string'
  ;; at 'pos' with its replacement.
  ;; See also 'repl-substring-list?'.
  ;; /DESC
  ;; /REFENTRY
  (if (repl-substring-list? string replace-list pos)
      (let ((target (repl-substring-list-target string replace-list pos))
	    (repl   (repl-substring-list-repl string replace-list pos)))
	(repl-substring string target repl pos))
      string))

(define (string-replace string target repl)
  ;; REFENTRY string-replace
  ;; PURP Replace all occurances of a target substring in a string
  ;; DESC
  ;; Replaces all occurances of 'target' in 'string' with 'repl'.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((str string) (pos 0))
    (if (>= pos (string-length str))
	str
	(loop (repl-substring str target repl pos) 
	      (if (repl-substring? str target pos)
		  (+ (string-length repl) pos)
		  (+ 1 pos))))))

(define (string-replace-list string replace-list)
  ;; REFENTRY string-replace-list
  ;; PURP Replace a list of target substrings in a string
  ;; DESC
  ;; Replaces, in 'string', all occurances of each target in
  ;; 'replace-list' with its replacement.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((str string) (pos 0))
    (if (>= pos (string-length str))
	str
	(loop (repl-substring-list str replace-list pos) 
	      (if (repl-substring-list? str replace-list pos)
		  (+ (string-length 
		      (repl-substring-list-repl str replace-list pos)) 
		     pos)
		  (+ 1 pos))))))

;; ======================================================================

(define (ancestor-member nd gilist)
  ;; REFENTRY ancestor-member
  ;; PURP Returns the first ancestor in a list of GIs
  ;; DESC
  ;; Returns the first ancestor of 'nd' whose GI that is a member of 'gilist'.
  ;; /DESC
  ;; /REFENTRY
  (if (node-list-empty? nd)
      (empty-node-list)
      (if (member (gi nd) gilist)
	  nd
	  (ancestor-member (parent nd) gilist))))

(define (has-ancestor-member? nd gilist)
  ;; REFENTRY has-ancestor-member-p
  ;; PURP Returns true if the specified node has one of a set of GIs as an ancestor
  ;; DESC
  ;; Returns '#t' if 'nd' has an ancestor whose GI is a member of 'gilist'.
  ;; /DESC
  ;; /REFENTRY
  (not (node-list-empty? (ancestor-member nd gilist))))

;; ======================================================================

(define (descendant-of? ancestor child)
  ;; REFENTRY descendant-of-p
  ;; PURP Returns true if the child is some descendant of the specified node
  ;; DESC
  ;; Returns '#t' if 'child' is a descendant of 'ancestor'.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((c child))
    (if (node-list-empty? c)
	#f
	(if (node-list=? ancestor c)
	    #t
	    (loop (parent c))))))

;; ======================================================================

(define (expand-children nodelist gilist)
  ;; REFENTRY expand-children
  ;; PURP Expand selected nodes in a node list
  ;; DESC
  ;; Given a node-list, 'expand-children' replaces all of the members
  ;; of the node-list whose GIs are members of 'gilist' with 
  ;; '(children)'.
  ;;
  ;; This function can be used to selectively 
  ;; flatten the hierarchy of a document.
  ;; /DESC
  ;; EXAMPLE
  ;; Suppose that the node list is '(BOOKINFO PREFACE PART APPENDIX)'.
  ;; '(expand-children nl ("PART"))' might return
  ;; '(BOOKINFO PREFACE CHAPTER CHAPTER APPENDIX)'.
  ;; /EXAMPLE
  ;; /REFENTRY
  (let loop ((nl nodelist) (result (empty-node-list)))
    (if (node-list-empty? nl)
	result
	(if (member (gi (node-list-first nl)) gilist)
	    (loop (node-list-rest nl) 
		  (node-list result (children (node-list-first nl))))
	    (loop (node-list-rest nl)
		  (node-list result (node-list-first nl)))))))

;; ======================================================================

(define (directory-depth pathname)
  ;; REFENTRY directory-depth
  ;; PURP Count the directory depth of a path name
  ;; DESC
  ;; Returns the number of directory levels in 'pathname'
  ;;
  ;; The pathname must end in a filename.
  ;; Further, this function assumes that directories in a pathname are 
  ;; separated by forward slashes ("/").
  ;; /DESC
  ;; EXAMPLE
  ;; "filename" => 0, 
  ;; "foo/filename" => 1, 
  ;; "foo/bar/filename => 2, 
  ;; "foo/bar/../filename => 1.
  ;; /EXAMPLE
  ;; /REFENTRY
  (let loop ((count 0) (pathlist (match-split pathname "/")))
    (if (null? pathlist)
	(- count 1) ;; pathname should always end in a filename
	(if (or (equal? (car pathlist) "/") (equal? (car pathlist) "."))
	    (loop count (cdr pathlist))
	    (if (equal? (car pathlist) "..")
		(loop (- count 1) (cdr pathlist))
		(loop (+ count 1) (cdr pathlist)))))))


(define (file-extension filespec) 
  ;; REFENTRY file-extension
  ;; PURP Return the extension of a filename
  ;; DESC
  ;; Returns the extension of a filename.  The extension is the last
  ;; "."-delimited part of the name.  Returns "" if there is no period
  ;; in the filename.
  ;; /DESC
  ;; /REFENTRY
  (if (string? filespec)
      (let* ((pathparts (match-split filespec "/"))
	     (filename  (list-ref pathparts (- (length pathparts) 1)))
	     (fileparts (match-split filename "."))
	     (extension (list-ref fileparts (- (length fileparts) 1))))
	(if (> (length fileparts) 1)
	    extension
	    ""))
      ""))

;; ======================================================================

(define (copy-string string num)
  ;; REFENTRY copy-string
  ;; PURP Return a string duplicated a specified number of times
  ;; DESC
  ;; Copies 'string' 'num' times and returns the result.
  ;; /DESC
  ;; EXAMPLE
  ;; (copy-string "x" 3) returns "xxx"
  ;; /EXAMPLE
  ;; /REFENTRY
  (if (<= num 0)
      ""
      (let loop ((str string) (count (- num 1)))
	(if (<= count 0)
	    str
	    (loop (string-append str string) (- count 1))))))

;; ======================================================================

(define (node-list-filter-by-gi nodelist gilist)
  ;; REFENTRY node-list-filter-by-gi
  ;; PURP Returns selected elements from a node list
  ;; DESC
  ;; Returns a node list containing all the nodes from 'nodelist' whose
  ;; GIs are members of 'gilist'.  The order of nodes in the node list
  ;; is preserved.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((result (empty-node-list)) (nl nodelist))
    (if (node-list-empty? nl)
	result
	(if (member (gi (node-list-first nl)) gilist)
	    (loop (node-list result (node-list-first nl)) 
		  (node-list-rest nl))
	    (loop result (node-list-rest nl))))))

;; ======================================================================

(define (node-list-filter-by-not-gi nodelist gilist)
  ;; REFENTRY node-list-filter-by-not-gi
  ;; PURP Returns selected elements from a node list
  ;; DESC
  ;; Returns a node list containing all the nodes from 'nodelist' whose
  ;; GIs are NOT members of 'gilist'.  The order of nodes in the node list
  ;; is preserved.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((result (empty-node-list)) (nl nodelist))
    (if (node-list-empty? nl)
	result
	(if (member (gi (node-list-first nl)) gilist)
	    (loop result (node-list-rest nl))
	    (loop (node-list result (node-list-first nl))
		  (node-list-rest nl))))))

;; ======================================================================

(define (node-list-filter-out-pis nodelist)
  ;; REFENTRY node-list-filter-out-pis
  ;; PURP Returns the nodelist with all PIs removed
  ;; DESC
  ;; Returns a node list containing all the nodes from 'nodelist' that
  ;; are not PIs.  The order of nodes in the node list is preserved.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((result (empty-node-list)) (nl nodelist))
    (if (node-list-empty? nl)
	result
	(if (equal? (node-property 'class-name (node-list-first nl)) 'pi)
	    (loop result (node-list-rest nl))
	    (loop (node-list result (node-list-first nl))
		  (node-list-rest nl))))))

;; ======================================================================

(define (node-list-filter-elements nodelist)
  ;; REFENTRY node-list-filter-elements
  ;; PURP Returns the elements in 'nodelist'
  ;; DESC
  ;; Returns the elements in 'nodelist'
  ;; /DESC
  ;; /REFENTRY
  (let loop ((result (empty-node-list)) (nl nodelist))
    (if (node-list-empty? nl)
	result
	(if (equal? (node-property 'class-name (node-list-first nl)) 'element)
	    (loop (node-list result (node-list-first nl))
		  (node-list-rest nl))
	    (loop result (node-list-rest nl))))))

;; ======================================================================

(define (component-descendant-node-list inputnd complist)
  ;; REFENTRY component-descendant-node-list
  ;; PURP Find all 'inputnd's within an ancestor element
  ;; DESC
  ;; Finds the first ancestor of 'inputnd' in 'complist' and then returns
  ;; a node list of all the 'inputnd's within (that are descendants of) 
  ;; that ancestor.
  ;; /DESC
  ;; /REFENTRY
  (let ((nd (ancestor-member inputnd complist)))
    (select-elements (descendants nd) (gi inputnd))))

(define (component-child-number inputnd complist)
  ;; REFENTRY component-child-number
  ;; PURP Find child-number within a component
  ;; DESC
  ;; Finds the first ancestor of 'inputnd' in 'complist' and then counts 
  ;; all the elements of type 'inputnd' from that point on and returns
  ;; the number of 'inputnd'.  (This is like a 'recursive-child-number'
  ;; starting at the first parent of 'inputnd' in 'complist'.)
  ;; /DESC
  ;; /REFENTRY
  (let loop ((nl (component-descendant-node-list inputnd complist))
	     (num 1))
    (if (node-list-empty? nl)
	0
	(if (node-list=? (node-list-first nl) inputnd)
	    num
	    (if (string=? (gi (node-list-first nl)) (gi inputnd))
		(loop (node-list-rest nl) (+ num 1))
		(loop (node-list-rest nl) num))))))

(define (component-list-descendant-node-list inputnd inputlist complist)
  ;; REFENTRY component-descendant-list-node-list
  ;; PURP Find all elements of a list of elements in a component
  ;; DESC
  ;; Finds the first ancestor of 'inputnd' in 'complist' and
  ;; then returns a list of all the elements in 'inputlist' 
  ;; within that component.
  ;;
  ;; WARNING: this requires walking over *all* the descendants
  ;; of the ancestor node. This may be *slow*.
  ;; /DESC
  ;; /REFENTRY
  (let ((nd (ancestor-member inputnd complist)))
    (let loop ((nl (descendants nd)) (result (empty-node-list)))
      (if (node-list-empty? nl)
	  result
	  (if (member (gi (node-list-first nl)) inputlist)
	      (loop (node-list-rest nl) 
		    (node-list result (node-list-first nl)))
	      (loop (node-list-rest nl) 
		    result))))))

(define (component-list-child-number inputnd inputlist complist)
  ;; REFENTRY component-list-child-number
  ;; PURP Find child-number of a list of children within a component
  ;; DESC
  ;; Finds the first ancestor of 'inputnd' in 'complist' and
  ;; then counts all the elements of the types in 'inputlist'
  ;; from that point on and returns the number of 'inputnd'.
  ;;
  ;; If the node is not found, 0 is returned.
  ;;
  ;; WARNING: this requires walking over *all* the descendants
  ;; of the ancestor node. This may be *slow*.
  ;; /DESC
  ;; /REFENTRY
    (let loop ((nl (component-list-descendant-node-list
		    inputnd inputlist complist))
	       (num 1))
      (if (node-list-empty? nl)
	  0
	  (if (node-list=? (node-list-first nl) inputnd)
	      num
	      (loop (node-list-rest nl) (+ num 1))))))

;; ======================================================================

(define (expt b n)
  ;; REFENTRY expt
  ;; PURP Exponentiation
  ;; DESC
  ;; Returns 'b' raised to the 'n'th power for integer 'n' >= 0.
  ;; /DESC
  ;; /REFENTRY
  ;; 
  (if (<= n 0)
      1
      (* b (expt b (- n 1)))))

;; ======================================================================

(define (list-member-find element elementlist)
  ;; REFENTRY list-member-find
  ;; PURP Returns the index of an element in a list
  ;; DESC
  ;; Returns the index of 'element' in the list 'elementlist'. The
  ;; first element in a list has index 0.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((elemlist elementlist) (count 0))
    (if (null? elemlist)
	-1
	(if (equal? element (car elemlist))
	    count
	    (loop (cdr elemlist) (+ count 1))))))

;; ======================================================================

(define default-uppercase-list
  ;; REFENTRY
  ;; PURP The default list of uppercase characters
  ;; DESC
  ;; The default list of uppercase characters.  The order and sequence
  ;; of characters
  ;; in this list must match the order and sequence in 
  ;; 'default-lowercase-list'.
  ;; /DESC
  ;; /REFENTRY
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define default-lowercase-list
  ;; REFENTRY
  ;; PURP The default list of lowercase characters
  ;; DESC
  ;; The default list of lowercase characters.  The order and sequence
  ;; of characters
  ;; in this list must match the order and sequence in 
  ;; 'default-uppercase-list'.
  ;; /DESC
  ;; /REFENTRY
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))


(define (case-fold-down-char ch #!optional (uc-list default-uppercase-list)
					   (lc-list default-lowercase-list))
  ;; REFENTRY
  ;; PURP Return the lowercase form of a single character
  ;; DESC
  ;; Returns the lowercase form of 'ch' if 'ch' is a member of
  ;; the uppercase list, otherwise return 'ch'.
  ;;
  ;; The implied mapping from uppercase to lowercase in the two lists is
  ;; one-to-one.  The first element of the uppercase list is the uppercase
  ;; form of the first element of the lowercase list, and vice versa.
  ;; ARGS
  ;; ARG 'ch'
  ;; The character to fold down.
  ;; /ARG
  ;; ARG 'uc-list' o
  ;; The list of uppercase letters. The default is the list of English 
  ;; uppercase letters.
  ;; /ARG
  ;; ARG 'lc-list' o
  ;; The list of lowercase letters. The default is the list of English 
  ;; lowercase letters.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (let ((idx (list-member-find ch uc-list)))
    (if (>= idx 0)
	(list-ref lc-list idx)
	ch)))

(define (case-fold-up-char ch #!optional (uc-list default-uppercase-list)
					 (lc-list default-lowercase-list))
  ;; REFENTRY
  ;; PURP Return the uppercase form of a single character
  ;; DESC
  ;; Returns the uppercase form of 'ch' if 'ch' is a member of
  ;; 'lowercase-list', otherwise return 'ch'.
  ;;
  ;; The implied mapping from uppercase to lowercase in the two lists is
  ;; one-to-one.  The first element of the uppercase list is the uppercase
  ;; form of the first element of the lowercase list, and vice versa.
  ;; ARGS
  ;; ARG 'ch'
  ;; The character to fold down.
  ;; /ARG
  ;; ARG 'uc-list' o
  ;; The list of uppercase letters. The default is the list of English 
  ;; uppercase letters.
  ;; /ARG
  ;; ARG 'lc-list' o
  ;; The list of lowercase letters. The default is the list of English 
  ;; lowercase letters.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (let ((idx (list-member-find ch lc-list)))
    (if (>= idx 0)
	(list-ref uc-list idx)
	ch)))

(define (case-fold-down-charlist charlist)
  ;; REFENTRY case-fold-down-charlist
  ;; PURP Return the list of characters, shifted to lowercase
  ;; DESC
  ;; Shifts all of the characters in 'charlist' to lowercase with
  ;; 'case-fold-down-char'.
  ;; /DESC
  ;; /REFENTRY
  (if (null? charlist)
      '()
      (cons (case-fold-down-char (car charlist)) 
	    (case-fold-down-charlist (cdr charlist)))))

(define (case-fold-up-charlist charlist)
  ;; REFENTRY case-fold-up-charlist
  ;; PURP Return the list of characters, shifted to uppercase
  ;; DESC
  ;; Shifts all of the characters in 'charlist' to uppercase with
  ;; 'case-fold-up-char'.
  ;; /DESC
  ;; /REFENTRY
  (if (null? charlist)
      '()
      (cons (case-fold-up-char (car charlist)) 
	    (case-fold-up-charlist (cdr charlist)))))

(define (case-fold-down str)
  ;; REFENTRY case-fold-down
  ;; PURP Shift a string to lowercase
  ;; DESC
  ;; Returns 'str' in lowercase.
  ;; /REFENTRY
  (if (string? str)
      (apply string (case-fold-down-charlist (string->list str)))
      str))

(define (case-fold-up str)
  ;; REFENTRY case-fold-up
  ;; PURP Shift a string to uppercase
  ;; DESC
  ;; Returns 'str' in uppercase.
  ;; /REFENTRY
  (if (string? str)
      (apply string (case-fold-up-charlist (string->list str)))
      str))

;; ======================================================================

(define (find-first-char string skipchars findchars #!optional (pos 0))
  ;; REFENTRY find-first-char
  ;; PURP Find the first occurance of a character in a string
  ;; DESC
  ;; Finds first character in 'string' that is in 'findchars', skipping all
  ;; occurances of characters in 'skipchars'.  Search begins at 'pos'.  If
  ;; no such characters are found, returns -1.
  ;;
  ;; If skipchars is empty, skip anything not in findchars
  ;; If skipchars is #f, skip nothing
  ;; If findchars is empty, the first character not in skipchars is matched
  ;; It is an error if findchars is not a string.
  ;; It is an error if findchars is empty and skipchars is not a non-empty
  ;; string.
  ;; /DESC
  ;; /REFENTRY
  (let ((skiplist (if (string? skipchars)
		      (string->list skipchars)
		      '()))
	(findlist (string->list findchars)))
    (if (and (null? skiplist) (null? findlist))
	;; this is an error
	-2
	(if (or (>= pos (string-length string)) (< pos 0))
	    -1
	    (let ((ch (string-ref string pos)))
	      (if (null? skiplist) 
		  ;; try to find first
		  (if (member ch findlist)
		      pos
		      (if (string? skipchars)
			  (find-first-char string 
					   skipchars findchars (+ 1 pos))
			  -1))
		  ;; try to skip first
		  (if (member ch skiplist)
		      (find-first-char string skipchars findchars (+ 1 pos))
		      (if (or (member ch findlist) (null? findlist))
			  pos
			  -1))))))))

;; ======================================================================

(define (parse-measurement measure)
  ;; REFENTRY parse-measurement
  ;; PURP Parse a string containing a measurement and return the magnitude and units
  ;; DESC
  ;; Parse a string containing a measurement, e.g., '"3pi"' or '"2.5in"',
  ;; and return the magnitude and units: '(3 "pi")' or '(2.5 "in")'.
  ;;
  ;; Either element of the list may be '#f' if the string cannot reasonably
  ;; be parsed as a measurement.  Leading and trailing spaces are ignored.
  ;; /DESC
  ;; /REFENTRY
  (let* ((magstart  (find-first-char measure " " "0123456789."))
	 (unitstart (find-first-char measure " 0123456789." ""))
	 (unitend   (find-first-char measure "" " " unitstart))
	 (magnitude (if (< magstart 0)
			#f
			(if (< unitstart 0)
			    (substring measure 
				       magstart 
				       (string-length measure))
			    (substring measure magstart unitstart))))
	 (unit      (if (< unitstart 0)
			#f
			(if (< unitend 0)
			    (substring measure 
				       unitstart 
				       (string-length measure))
			    (substring measure unitstart unitend)))))
  (list magnitude unit)))

(define unit-conversion-alist
  ;; REFENTRY
  ;; PURP Defines the base length of specific unit names
  ;; DESC
  ;; This list identifies the length of each unit.
  ;; /DESC
  ;; /REFENTRY
  (list
   '("default" 1pi)
   '("mm" 1mm)
   '("cm" 1cm)
   '("in" 1in)
   '("pi" 1pi)
   '("pc" 1pi)
   '("pt" 1pt)
   '("px" 1px)
   '("barleycorn" 2pi)))

(define (measurement-to-length measure)
  ;; REFENTRY measurement-to-length
  ;; PURP Convert a measurement to a length
  ;; DESC
  ;; Given a string containing a measurement, return that measurement
  ;; as a length.
  ;; /DESC
  ;; EXAMPLES
  ;; '"2.5cm"' returns 2.5cm as a length.  '"3.4barleycorn"' returns
  ;; 6.8pi.
  ;; /EXAMPLES
  ;; /REFENTRY
  (let* ((pm (car (parse-measurement measure)))
	 (pu (car (cdr (parse-measurement measure))))
	 (magnitude (if pm pm "1"))
	 (units     (if pu pu (if pm "pt" "default")))
	 (unitconv  (assoc units unit-conversion-alist))
	 (factor    (if unitconv (car (cdr unitconv)) 1pt)))
    (* (string->number magnitude) factor)))

;; ======================================================================

(define (dingbat usrname)
  ;; REFENTRY dingbat
  ;; PURP Map dingbat names to Unicode characters
  ;; DESC
  ;; Map a dingbat name to the appropriate Unicode character.
  ;; /DESC
  ;; /REFENTRY
  ;; Print dingbats and other characters selected by name
  (let ((name (case-fold-down usrname)))
    (case name
      ;; For backward compatibility
      (("box") 			"\white-square;")
      (("checkbox")		"\white-square;")
      ;; \check-mark prints the wrong symbol (in Jade 0.8 RTF backend)
      (("check")		"\heavy-check-mark;") 
      (("checkedbox")		"\ballot-box-with-check;")
      (("dash")			"\em-dash;")
      (("copyright")		"\copyright-sign")

      ;; Straight out of Unicode
      (("raquo")                "\U-00BB;")
      (("laquo")                "\U-00AB;")
      (("rsaquo")               "\U-203A;")
      (("lsaquo")               "\U-2039;")
      (("lsquo")		"\U-2018;")
      (("rsquo")		"\U-2019;")
      (("ldquo")		"\U-201C;")
      (("rdquo")		"\U-201D;")
      (("ldquor")		"\U-201E;")
      (("rdquor")		"\U-201D;")
      (("en-dash")		"\en-dash;")
      (("em-dash")		"\em-dash;")
      (("en-space")		"\U-2002;")
      (("em-space")		"\U-2003;")
      (("bullet")		"\bullet;")
      (("black-square")		"\black-square;")
      (("white-square")		"\white-square;")
      ;; \ballot-box name doesn't work (in Jade 0.8 RTF backend)
      ;; and \white-square looks better than \U-2610; anyway
      (("ballot-box")		"\white-square;")
      (("ballot-box-with-check")	"\ballot-box-with-check;")
      (("ballot-box-with-x")	"\ballot-box-with-x;")
      ;; \check-mark prints the wrong symbol (in Jade 0.8 RTF backend)
      (("check-mark")		"\heavy-check-mark;") 
      ;; \ballot-x prints out the wrong symbol (in Jade 0.8 RTF backend)
      (("ballot-x")		"\heavy-check-mark;")
      (("copyright-sign")	"\copyright-sign;")
      (("registered-sign")	"\registered-sign;")
      (else "\bullet;"))))

;; ======================================================================

(define (nth-node nl k)
  ;; REFENTRY nth-node
  ;; PURP Return a specific node in a node list (by numeric index)
  ;; DESC
  ;; Returns the 'k'th node in 'nl'.  The first node in the node list
  ;; has the index "1".
  ;; /DESC
  ;; /REFENTRY
  (if (equal? k 1)
      (node-list-first nl)
      (nth-node (node-list-rest nl) (- k 1))))

;; ======================================================================

(define (constant-list value length)
  ;; REFENTRY constant-list
  ;; PURP Returns a list of the specified value
  ;; DESC
  ;; Return a list containing 'length' elements, each of 'value'.
  ;; /DESC
  ;; AUTHOR David Carlisle
  ;; EXAMPLE
  ;; '(constant-list 0 4)' returns '(0 0 0 0)'
  ;; /EXAMPLE
  ;; /REFENTRY
   (let loop ((count (abs length)) (result '()))
     (if (equal? count 0)
         result
         (loop (- count 1) (cons value  result)))))

(define (list-head inputlist k)
  ;; REFENTRY list-head
  ;; PURP Return the head of a list
  ;; DESC
  ;; Returns the list that contains the first 'k' elements of 'inputlist'.
  ;; /DESC
  ;; EXAMPLE
  ;; '(list-head (1 2 3 4) 2)' returns '(1 2)'.
  ;; /EXAMPLE
  ;; /REFENTRY
  (let loop ((l inputlist) (count k) (result '()))
    (if (<= count 0)
	result
	(loop (cdr l) (- count 1) (append result (list (car l)))))))

(define (list-put vlist ordinal value #!optional (span 1))
  ;; REFENTRY list-put
  ;; PURP Replace a specific member of a list
  ;; DESC
  ;; Replaces the 'ordinal'th value of 'vlist' with 'value'.  If 'span' > 1,
  ;; replaces 'ordinal' to 'ordinal+span-1' values starting at 'ordinal'.
  ;; /DESC
  ;; EXAMPLE
  ;; '(list-put (1 2 3 4 5) 2 0 2)' returns '(1 0 0 4 5)'.
  ;; /EXAMPLE
  ;; /REFENTRY
  (let loop ((result vlist) (count span) (k ordinal))
    (if (equal? count 0)
	result
	(let ((head (list-head result (- k 1)))
	      (tail (list-tail result k)))
	  (loop (append head (list value) tail) (- count 1) (+ k 1))))))

(define (decrement-list-members vlist #!optional (decr 1) (floor 0))
  ;; REFENTRY decrement-list-members
  ;; PURP Decrement each member of a list
  ;; DESC
  ;; Decrement all the values of a list by 'decr', not to fall below 'floor'.
  ;; ARGS
  ;; ARG 'vlist'
  ;; The list of values.  All the values of this list should be numeric.
  ;; /ARG
  ;; ARG 'decr' o
  ;; The amount by which each element of the list should be decremented.
  ;; The default is 1.
  ;; /ARG
  ;; ARG 'floor' o
  ;; The value below which each member of the list is not allowed to fall.
  ;; The default is 0.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; AUTHOR David Carlisle
  ;; EXAMPLE
  ;; '(decrement-list-members (0 1 2 3 4 5))' => '(0 0 1 2 3 4)'.
  ;; /EXAMPLE
  ;; /REFENTRY
  (map (lambda (a) 
	 (if (<= a (+ decr floor))
	     floor
	     (- a decr)))
       vlist))

;; ======================================================================

(define (sgml-root-element #!optional (grove-node (current-node)))
  ;; REFENTRY
  ;; PURP Returns the node that is the root element of the current document
  ;; DESC
  ;; Returns the node that is the root element of the current document
  ;; /DESC
  ;; /REFENTRY
  (node-property 'document-element (node-property 'grove-root grove-node)))

(define (sgml-root-element? node)
  ;; REFENTRY
  ;; PURP Test if a node is the root element
  ;; DESC
  ;; Returns '#t' if node is the root element of the current document.
  ;; /DESC
  ;; /REFENTRY
  (node-list=? node (sgml-root-element node)))

;; ======================================================================

(define (length-string-number-part lenstr)
  ;; REFENTRY length-string-number-part
  ;; PURP Returns the numeric part of a length string
  ;; DESC
  ;; Given a length as a string, return the numeric part.
  ;; /DESC
  ;; EXAMPLE
  ;; '"100pt"' returns '"100"'. '"30"' returns '"30"'.  
  ;; '"in"' returns '""'.
  ;; /EXAMPLE
  ;; /REFENTRY
  (let ((digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)))
    (let loop ((chars (string->list lenstr))
	       (number-part ""))
      (if (or (null? chars) (not (member (car chars) digits)))
	  number-part
	  (loop (cdr chars) (string-append number-part 
					   (string (car chars))))))))

(define (length-string-unit-part lenstr)
  ;; REFENTRY length-string-unit-part
  ;; PURP Returns the unit part of a length string
  ;; DESC
  ;; Given a length as a string, return the units part.
  ;; /DESC
  ;; EXAMPLE
  ;; '"100pt"' returns '"pt"'. '"30"' returns '""'.  
  ;; '"in"' returns '"in"'.
  ;; /EXAMPLE
  ;; /REFENTRY
  (let ((number-part (length-string-number-part lenstr))
	(strlen (string-length lenstr)))
    (if (equal? (string-length number-part) strlen)
	""
	(substring lenstr (string-length number-part) strlen))))

;; ======================================================================

(define (normalize str)
  ;; REFENTRY normalize
  ;; PURP Normalize the str according to the SGML declaration in effect
  ;; DESC
  ;; Performs SGML general name normalization on the string;
  ;; used to compare attribute names and generic identifiers correctly
  ;; according to the SGML declaration in effect; this is necessary
  ;; since XML is case-sensitive but the reference concrete syntax and
  ;; many SGML DTDs are not.
  ;; /DESC
  ;; AUTHOR Chris Maden
  ;; /REFENTRY
  (if (string? str)
      (general-name-normalize str
			      (current-node))
      str))

;; ======================================================================

(define (node-list->string nodelist)
  ;; REFENTRY node-2-string
  ;; PURP Return a string representation of the node list
  ;; DESC
  ;; Builds a string representation of the node list and returns it.
  ;; The representation is 
  ;;
  ;; "gi(firstchildgi()secondchildgi(firstgrandchildgi())) secondgi()..."
  ;;
  ;; This is a debugging function, in case that wasn't obvious...
  ;; /DESC
  ;; /REFENTRY
  (let loop ((nl nodelist) (res ""))
    (if (node-list-empty? nl)
	res
	(loop (node-list-rest nl)
	      (string-append res 
			     (if (gi (node-list-first nl))
				 (string-append
				  (gi (node-list-first nl))
				  "("
				  (node-list->string 
				   (children (node-list-first nl)))
				  ")")
				 ""))))))

;; ======================================================================

(define (include-file fileref)
  ;; REFENTRY include-file
  ;; PURP Return the literal content of fileref
  ;; DESC
  ;; Opens and loads fileref with (read-entity); returns the content
  ;; of fileref as a (literal).  Trims the last trailing newline off
  ;; the file so that "the right thing" happens in asis environments.
  ;; /DESC
  ;; /REFENTRY
  (literal (include-characters fileref)))

;; ======================================================================

(define (include-characters fileref)
  ;; REFENTRY include-characters
  ;; PURP Return the character content of fileref
  ;; DESC
  ;; Opens and loads fileref with (read-entity); returns the content
  ;; of fileref as characters.  Trims the last trailing newline off
  ;; the file so that "the right thing" happens in asis environments.
  ;; /DESC
  ;; /REFENTRY
  (let* ((newline #\U-000D)
	 (file-content  (read-entity fileref))
	 (file-length   (string-length file-content))
	 ;; If the last char is a newline, drop it, otherwise print it...
	 (content       (if (equal? newline (string-ref file-content
							(- file-length 1)))
			    (substring file-content 0 (- file-length 1))
			    file-content)))
    content))

;; ======================================================================

(define (url-encode-char ch)
  ;; REFENTRY url-encode-char
  ;; PURP Returns the url-encoded equivalent of a character
  ;; DESC
  ;; Converts 'ch' to a properly encoded URL character.
  ;; /DESC
  ;; /REFENTRY
  (cond ((char=? ch #\space)  "%20") ; space
	((char=? ch #\U-0026) "%26") ; ampersand
	((char=? ch #\?)      "%3F") ; question
	((char=? ch #\{)      "%7B") ; open curly
	((char=? ch #\})      "%7D") ; close curly
	((char=? ch #\|)      "%7C") ; vertical bar
	((char=? ch #\\)      "%5C") ; backslash
	((char=? ch #\/)      "%2F") ; slash
	((char=? ch #\^)      "%5E") ; caret
	((char=? ch #\~)      "%7E") ; tilde
	((char=? ch #\[)      "%5B") ; open square
	((char=? ch #\])      "%5D") ; close square
	((char=? ch #\`)      "%60") ; backtick
	((char=? ch #\%)      "%25") ; percent
	((char=? ch #\+)      "%2B") ; plus
	(else (string ch))))

(define (url-encode-string str)
  ;; REFENTRY url-encode-string
  ;; PURP Returns str with all special characters %-encoded
  ;; DESC
  ;; Converts 'str' to a properly encoded URL string.  Returns str unchanged
  ;; if it is not a string.
  ;; /DESC
  ;; /REFENTRY
  (if (string? str)
      (let loop ((charlist (string->list str)) (url ""))
	(if (null? charlist)
	    url
	    (loop (cdr charlist) 
		  (string-append url (url-encode-char (car charlist))))))
      str))

;; ======================================================================

(define (system-id-filename target)
  ;; REFENTRY system-id-filename
  ;; PURP Returns the filename part of the system id of target
  ;; DESC
  ;; The entity-generated-system-id of target seems to begin with a
  ;; keyword, usually OSFILE on my system, in angle brackets.
  ;; This function removes the leading OSFILE bit.
  ;; /DESC
  ;; /REFENTRY
  (let* ((sysid  (entity-generated-system-id target))
	 (fnbits (split sysid '(#\>)))
	 (fntail (cdr fnbits)))
    (join fntail "\U-0061;")))

;; ======================================================================

(define (trim-string str string-list)
  ;; REFENTRY trim-string
  ;; PURP Trims the tail off of a string
  ;; DESC
  ;; If 'str' ends with any of the strings in 'string-list', trim that
  ;; string off and return the base string.
  ;; E.g., '(trim-string "filename.sgm" '(".sgm" ".xml" ".sgml"))
  ;; returns "filename".
  ;; /DESC
  ;; /REFENTRY
  (let ((strlen (string-length str)))
    (let loop ((sl string-list))
      (if (null? sl)
	  str
	  (if (equal? 
	       (substring str (- strlen (string-length (car sl))) strlen)
	       (car sl))
	      (substring str 0 (- strlen (string-length (car sl))))
	      (loop (cdr sl)))))))

;; ======================================================================

(define (string-index source target)
  ;; REFENTRY string-index
  ;; PURP Finds first occurance of 'target' in 'source'
  ;; DESC
  ;; Returns the position of the first occurance of 'target' in 'source',
  ;; or -1 if it does not occur.
  ;; /DESC
  ;; /REFENTRY
  (let loop ((str source) (pos 0))
    (if (< (string-length str) (string-length target))
	-1
	(if (string=? (substring str 0 (string-length target)) target)
	    pos
	    (loop (substring str 1 (string-length str))
		  (+ pos 1))))))

;; ======================================================================

(define (parse-pi-attribute pivalues #!optional (skip #f))
  (let* ((equalpos (string-index pivalues "="))
	 (name     (substring pivalues 0 equalpos))
	 (quotchar (substring pivalues (+ equalpos 1) (+ equalpos 2)))
	 (rest     (substring pivalues 
			      (+ equalpos 2) 
			      (string-length pivalues)))
	 (quotpos  (string-index rest quotchar))
	 (value    (substring rest 0 quotpos))
	 (morevals (strip (substring rest 
				     (+ quotpos 1) 
				     (string-length rest)))))
    (if skip
	morevals
	(list name value))))

(define (parse-skip-pi-attribute pivalues)
  (parse-pi-attribute pivalues #t))

(define (parse-starttag-pi pi)
  ;; REFENTRY parse-starttag-pi
  ;; PURP Parses a structured PI and returns a list of values
  ;; DESC
  ;; It has become common practice to give PIs structured values.  The
  ;; resultis a PI that looks a lot like a start tag with attributes:
  ;;
  ;; &#60;?pitarget name1="value1" name2='value2' name3="value '3'">
  ;; 
  ;; This function parses a PI with this form and returns a list. The
  ;; list contains the pitarget and each of the name/value pairs:
  ;;
  ;; ("pitarget" "name1" "value1" "name2" "value2" "name3" "value '3'")
  ;; /DESC
  ;; /REFENTRY
  (let* ((strippi (strip pi))
	 (spacepos (string-index strippi " ")))
    (if (< spacepos 0)
	(list strippi)
	(let* ((pitarget (substring strippi 0 spacepos))
	       (pivalues (strip (substring strippi 
					   (+ spacepos 1)
					   (string-length strippi)))))
	  (let loop ((values pivalues) (result (list pitarget)))
	    (if (string=? values "")
		result
		(loop (parse-skip-pi-attribute values)
		      (append result (parse-pi-attribute values)))))))))

;; ======================================================================

(define (string->nodes s)
  ;; Escape XML characters...
  (let* ((achars (string-replace s "&#38;" "&#38;#38;#38;"))
	 (bchars (string-replace achars "&#60;" "&#38;#38;#60;"))
	 (cchars (string-replace bchars "&#62;" "&#38;#38;#62;")))
    (let ((doc (string-append "&#60;literal>&#60;!DOCTYPE doc [ &#60;!ELEMENT "
			      "doc - - (#PCDATA)> ]>&#60;doc>" cchars ";&#60;/doc>")))
      (children (node-property 'docelem (sgml-parse doc))))))

;; ======================================================================

</style-specification-body>
</style-specification>
</style-sheet>
