;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; ==================== GRAPHICS ====================

;; NOTE: display #f doesn't seem to work right in the RTF back end...

(define (graphic-file filename)
  (let ((ext (file-extension filename)))
    (if (or (not filename)
	    (not %graphic-default-extension%)
	    (member ext %graphic-extensions%))
	filename
	(string-append filename "." %graphic-default-extension%))))

(define ($graphic$ fileref 
		   #!optional (display #f) (format #f) (scale #f) (align #f))
  (let ((graphic-format (if format format ""))
	(graphic-scale  (if scale (/  (string->number scale) 100) 1))
	(graphic-align  (cond ((equal? align (normalize "center"))
			       'center)
			      ((equal? align (normalize "right"))
			       'end)
			      (else
			       'start))))
    (make external-graphic
      entity-system-id: (graphic-file fileref)
      notation-system-id: graphic-format
      scale: graphic-scale
      display?: display
      display-alignment: graphic-align)))

(define ($img$ #!optional (nd (current-node)) (display #f))
  ;; This function now supports an extension to DocBook.  It's
  ;; either a clever trick or an ugly hack, depending on your
  ;; point of view, but it'll hold us until XLink is finalized
  ;; and we can extend DocBook the "right" way.
  ;;
  ;; If the entity passed to GRAPHIC has the FORMAT
  ;; "LINESPECIFIC", either because that's what's specified or
  ;; because it's the notation of the supplied ENTITYREF, then
  ;; the text of the entity is inserted literally (via Jade's
  ;; read-entity external procedure).
  ;;
  (let* ((fileref   (attribute-string (normalize "fileref") nd))
	 (entityref (attribute-string (normalize "entityref") nd))
	 (format    (if (attribute-string (normalize "format") nd)
			(attribute-string (normalize "format") nd)
			(if entityref
			    (entity-notation entityref)
			    #f)))
	 (align     (attribute-string (normalize "align") nd))
	 (scale     (attribute-string (normalize "scale") nd)))
    (if (or fileref entityref)
	(if (equal? format (normalize "linespecific"))
	    (if fileref
		(include-file fileref)
		(include-file (entity-generated-system-id entityref)))
	    (if fileref
		($graphic$ fileref display format scale align)
		($graphic$ (entity-generated-system-id entityref)
			   display format scale align)))
	(empty-sosofo))))

(element graphic
  (make paragraph
    space-before: %block-sep%
    space-after: %block-sep%
    ($img$ (current-node) #t)))

(element inlinegraphic ($img$))
