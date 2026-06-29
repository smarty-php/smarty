getTemplateVars()

returns assigned variable value(s)

Description
===========

array

getTemplateVars

string

varname

If no parameter is given, an array of all [assigned](../../programmers/api-functions/api-assign.md)
variables are returned.


    <?php
    // get assigned template var 'foo'
    $myVar = $smarty->getTemplateVars('foo');

    // get all assigned template vars
    $all_tpl_vars = $smarty->getTemplateVars();

    // take a look at them
    print_r($all_tpl_vars);
    ?>

       

See also [`assign()`](../../programmers/api-functions/api-assign.md),
[`{assign}`](../../designers/language-builtin-functions/language-function-assign.md), [`append()`](../../programmers/api-functions/api-append.md),
[`clearAssign()`](../../programmers/api-functions/api-clear-assign.md),
[`clearAllAssign()`](../../programmers/api-functions/api-clear-all-assign.md) and
[`getConfigVars()`](../../programmers/api-functions/api-get-config-vars.md)
