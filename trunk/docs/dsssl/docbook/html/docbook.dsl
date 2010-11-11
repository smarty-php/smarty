<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [

<!ENTITY % dbl10n.ent SYSTEM "../common/dbl10n.ent">
%dbl10n.ent;

<!ENTITY dbl10n.dsl  SYSTEM "../common/dbl10n.dsl">

<!ENTITY dblib.dsl
  PUBLIC "-//Norman Walsh//DOCUMENT DSSSL Library V2//EN" CDATA DSSSL>

<!ENTITY dbparam.dsl  SYSTEM "dbparam.dsl" CDATA DSSSL>

<!ENTITY VERSION SYSTEM "../VERSION">

<!ENTITY dbcommon.dsl SYSTEM "../common/dbcommon.dsl">
<!ENTITY dbctable.dsl SYSTEM "../common/dbtable.dsl">
<!ENTITY dbadmon.dsl  SYSTEM "dbadmon.dsl">
<!ENTITY dbautoc.dsl  SYSTEM "dbautoc.dsl">
<!ENTITY dbbibl.dsl   SYSTEM "dbbibl.dsl">
<!ENTITY dbblock.dsl  SYSTEM "dbblock.dsl">
<!ENTITY dbcallou.dsl SYSTEM "dbcallou.dsl">
<!ENTITY dbcompon.dsl SYSTEM "dbcompon.dsl">
<!ENTITY dbdivis.dsl  SYSTEM "dbdivis.dsl">
<!ENTITY dbfootn.dsl  SYSTEM "dbfootn.dsl">
<!ENTITY dbgloss.dsl  SYSTEM "dbgloss.dsl">
<!ENTITY dbgraph.dsl  SYSTEM "dbgraph.dsl">
<!ENTITY dbhtml.dsl   SYSTEM "dbhtml.dsl">
<!ENTITY dbindex.dsl  SYSTEM "dbindex.dsl">
<!ENTITY dbinfo.dsl   SYSTEM "dbinfo.dsl">
<!ENTITY dbinline.dsl SYSTEM "dbinline.dsl">
<!ENTITY dblink.dsl   SYSTEM "dblink.dsl">
<!ENTITY dblists.dsl  SYSTEM "dblists.dsl">
<!ENTITY dblot.dsl    SYSTEM "dblot.dsl">
<!ENTITY dbmath.dsl   SYSTEM "dbmath.dsl">
<!ENTITY dbmsgset.dsl SYSTEM "dbmsgset.dsl">
<!ENTITY dbnavig.dsl  SYSTEM "dbnavig.dsl">
<!ENTITY dbchunk.dsl  SYSTEM "dbchunk.dsl">
<!ENTITY dbpi.dsl     SYSTEM "dbpi.dsl">
<!ENTITY dbprocdr.dsl SYSTEM "dbprocdr.dsl">
<!ENTITY dbrfntry.dsl SYSTEM "dbrfntry.dsl">
<!ENTITY dbsect.dsl   SYSTEM "dbsect.dsl">
<!ENTITY dbsynop.dsl  SYSTEM "dbsynop.dsl">
<!ENTITY dbefsyn.dsl  SYSTEM "dbefsyn.dsl">
<!ENTITY dbtable.dsl  SYSTEM "dbtable.dsl">
<!ENTITY dbtitle.dsl  SYSTEM "dbtitle.dsl">
<!ENTITY dbttlpg.dsl  SYSTEM "dbttlpg.dsl">
<!ENTITY dbverb.dsl   SYSTEM "dbverb.dsl">
<!ENTITY version.dsl  SYSTEM "version.dsl">
<!ENTITY db31.dsl     SYSTEM "db31.dsl">
]>

<style-sheet>
<style-specification id="docbook" 
    use="af ca cs da de el en es et eu fi fr hu id it ja ko nl nn no pl pt ptbr ro ru sk sl sr sv tr uk xh zhcn zhtw zhhk dbparam dblib">

<style-specification-body>

;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

(declare-flow-object-class element
  "UNREGISTERED::James Clark//Flow Object Class::element")

(declare-flow-object-class empty-element
  "UNREGISTERED::James Clark//Flow Object Class::empty-element")

(declare-flow-object-class document-type
  "UNREGISTERED::James Clark//Flow Object Class::document-type")

(declare-flow-object-class processing-instruction
  "UNREGISTERED::James Clark//Flow Object Class::processing-instruction")

(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")

(declare-flow-object-class entity-ref
  "UNREGISTERED::James Clark//Flow Object Class::entity-ref")

(declare-flow-object-class formatting-instruction
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")

(declare-characteristic preserve-sdata?
  "UNREGISTERED::James Clark//Characteristic::preserve-sdata?" #t)

(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))

(define read-entity
  (external-procedure "UNREGISTERED::James Clark//Procedure::read-entity"))

(define all-element-number
  (external-procedure "UNREGISTERED::James Clark//Procedure::all-element-number"))

(root
 (make sequence
;   (literal
;    (debug (node-property 'gi
;			  (node-property 'document-element (current-node)))))
;(define (docelem node)
;  (node-propety 'document-element 
;    (node-property 'grove-root node)))
   (process-children)
   (with-mode manifest
     (process-children))
   (if html-index
       (with-mode htmlindex
	 (process-children))
       (empty-sosofo))))

(mode manifest
  ;; this mode is really just a hack to get at the root element
  (root (process-children))

  (default 
    (if (node-list=? (current-node) (sgml-root-element))
	(if html-manifest
	    (make entity
	      system-id: (html-entity-file html-manifest-filename)
	      (make sequence
		(let loop ((node (current-node)))
		  (if (node-list-empty? node)
		      (empty-sosofo)
		      (make sequence
			(make formatting-instruction data: (html-file node))
			(make formatting-instruction data: "
")
			(loop (next-chunk-element node)))))
		(let loop ((nl (select-elements (descendants (current-node))
						(normalize "legalnotice"))))
		  (if (node-list-empty? nl)
		      (empty-sosofo)
		      (make sequence
			(if (and %generate-legalnotice-link%
				 (not nochunks)
				 (first-sibling? (node-list-first nl))
				 ;; Hack: only book legal notices are diverted
				 (have-ancestor? (normalize "bookinfo")
						 (node-list-first nl)))
			    (make sequence
			      (make formatting-instruction
				data: ($legalnotice-link-file$ (node-list-first nl)))
			      (make formatting-instruction data: "
"))
			    (empty-sosofo))
			(loop (node-list-rest nl)))))))
	    (empty-sosofo))
	(empty-sosofo))))

;; Make text that comes from unimplemented tags easy to spot
(default
  (make element gi: "FONT"
	attributes: '(("COLOR" "RED"))
	(process-children)))

&dbcommon.dsl;  <!-- Common ("stock") stylesheet functions                 -->
&dbctable.dsl;  <!-- Common stylesheet functions for tables                -->

&dbl10n.dsl;    <!-- Stylesheet-local localization -->

&dbadmon.dsl;   <!-- Admonitions                                           -->
&dbautoc.dsl;   <!-- Automatic TOC generation                              -->
&dbbibl.dsl;    <!-- Bibliographies                                        -->
&dbblock.dsl;   <!-- Miscellaneous block elements                          -->
&dbcallou.dsl;  <!-- Callouts                                              -->
&dbcompon.dsl;  <!-- Components; chapter-level elements                    -->
&dbdivis.dsl;   <!-- Divisions; Sets, Books, Articles, Parts               -->
&dbfootn.dsl;   <!-- Footnotes                                             -->
&dbgloss.dsl;   <!-- Glossaries                                            -->
&dbgraph.dsl;   <!-- Graphics                                              -->
&dbhtml.dsl;    <!-- HTML specific things                                  -->
&dbindex.dsl;   <!-- Indexes                                               -->
&dbinfo.dsl;    <!-- Infopools (SetInfo, BookInfo, Sect1Info, etc.)        -->
&dbinline.dsl;  <!-- Inline elements                                       -->
&dblink.dsl;    <!-- Links                                                 -->
&dblists.dsl;   <!-- Lists                                                 -->
&dblot.dsl;     <!-- Lists of Tables (ToC, LoT, etc.)                      -->
&dbmath.dsl;    <!-- Math (Equations)                                      -->
&dbmsgset.dsl;  <!-- MsgSet                                                -->
&dbnavig.dsl;   <!-- Navigation                                            -->
&dbchunk.dsl;   <!-- Chunking                                              -->
&dbpi.dsl;      <!-- HTML PIs (dbhtml)                                     -->
&dbprocdr.dsl;  <!-- Procedures                                            -->
&dbrfntry.dsl;  <!-- References and RefEntrys                              -->
&dbsect.dsl;    <!-- Sections                                              -->
&dbsynop.dsl;   <!-- Synopsis                                              -->
&dbefsyn.dsl;   <!-- ClassSynopsis                                         -->
&dbtable.dsl;   <!-- Tables                                                -->
&dbtitle.dsl;   <!-- Titles                                                -->
&dbttlpg.dsl;   <!-- Title Page                                            -->
&dbverb.dsl;    <!-- Verbatim (ProgramListing, LiteralLayout, etc.)        -->
&version.dsl;   <!-- Version -->
&db31.dsl;      <!-- DocBook 3.1 elements -->

</style-specification-body>
</style-specification>

<external-specification id="dbparam" document="dbparam.dsl">
<external-specification id="dblib" document="dblib.dsl">

<external-specification id="af" document="dbl1af">
<external-specification id="ca" document="dbl1ca">
<external-specification id="cs" document="dbl1cs">
<external-specification id="da" document="dbl1da">
<external-specification id="de" document="dbl1de">
<external-specification id="el" document="dbl1el">
<external-specification id="en" document="dbl1en">
<external-specification id="eu" document="dbl1eu">
<external-specification id="es" document="dbl1es">
<external-specification id="et" document="dbl1et">
<external-specification id="fi" document="dbl1fi">
<external-specification id="fr" document="dbl1fr">
<external-specification id="hu" document="dbl1hu">
<external-specification id="id" document="dbl1id">
<external-specification id="it" document="dbl1it">
<external-specification id="ja" document="dbl1ja">
<external-specification id="ko" document="dbl1ko">
<external-specification id="nl" document="dbl1nl">
<external-specification id="nn" document="dbl1nn">
<external-specification id="no" document="dbl1no">
<external-specification id="pl" document="dbl1pl">
<external-specification id="pt" document="dbl1pt">
<external-specification id="ptbr" document="dbl1ptbr">
<external-specification id="ro" document="dbl1ro">
<external-specification id="ru" document="dbl1ru">
<external-specification id="sk" document="dbl1sk">
<external-specification id="sl" document="dbl1sl">
<external-specification id="sr" document="dbl1sr">
<external-specification id="sv" document="dbl1sv">
<external-specification id="tr" document="dbl1tr">
<external-specification id="uk" document="dbl1uk">
<external-specification id="xh" document="dbl1xh">
<external-specification id="zhcn" document="dbl1zhcn">
<external-specification id="zhtw" document="dbl1zhtw">
<external-specification id="zhhk" document="dbl1zhhk">

</style-sheet>


