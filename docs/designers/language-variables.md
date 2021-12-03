Variables {#language.variables}
=========

Smarty has several different types of variables. The type of the
variable depends on what symbol it is prefixed or enclosed within.

Variables in Smarty can be either displayed directly or used as
arguments for [functions](#language.syntax.functions),
[attributes](#language.syntax.attributes) and
[modifiers](#language.modifiers), inside conditional expressions, etc.
To print a variable, simply enclose it in the
[delimiters](#variable.left.delimiter) so that it is the only thing
contained between them.


    {$Name}

    {$product.part_no} <b>{$product.description}</b>

    {$Contacts[row].Phone}

    <body bgcolor="{#bgcolor#}">

      

> **Note**
>
> An easy way to examine assigned Smarty variables is with the
> [debugging console](#chapter.debugging.console).

DESIGNERS.LANGUAGE-VARIABLES.LANGUAGE-ASSIGNED-VARIABLES
DESIGNERS.LANGUAGE-VARIABLES.LANGUAGE-VARIABLE-SCOPES
DESIGNERS.LANGUAGE-VARIABLES.LANGUAGE-CONFIG-VARIABLES
DESIGNERS.LANGUAGE-VARIABLES.LANGUAGE-VARIABLES-SMARTY
