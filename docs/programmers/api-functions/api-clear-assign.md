clearAssign()

clears the value of an assigned variable

Description
===========

void

clearAssign

mixed

var

This can be a single value, or an array of values.


    <?php
    // clear a single variable
    $smarty->clearAssign('Name');

    // clears multiple variables
    $smarty->clearAssign(array('Name', 'Address', 'Zip'));
    ?>

       

See also [`clearAllAssign()`](../../programmers/api-functions/api-clear-all-assign.md),
[`clearConfig()`](../../programmers/api-functions/api-clear-config.md),
[`getTemplateVars()`](../../programmers/api-functions/api-get-template-vars.md), 
[`assign()`](../../programmers/api-functions/api-assign.md)
and [`append()`](../../programmers/api-functions/api-append.md)
