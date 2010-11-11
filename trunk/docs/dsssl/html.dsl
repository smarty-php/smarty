<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook.dsl SYSTEM "./dsssl/docbook/html/docbook.dsl" CDATA DSSSL>
<!ENTITY html-common.dsl SYSTEM "html-common.dsl">
<!ENTITY common.dsl SYSTEM "common.dsl">
]>

<style-sheet>
<style-specification id="docbook-smarty-html" use="docbook">
<style-specification-body>

(define %html-ext% ".html")
(define %output-dir% "html")

&html-common.dsl;
&common.dsl;

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook.dsl">

</style-sheet>
