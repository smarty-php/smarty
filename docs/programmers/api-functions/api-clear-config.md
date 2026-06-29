clearConfig()

clears assigned config variables

Description
===========

void

clearConfig

string

var

This clears all assigned [config variables](../../designers/language-variables/language-config-variables.md).
If a variable name is supplied, only that variable is cleared.


    <?php
    // clear all assigned config variables.
    $smarty->clearConfig();

    // clear one variable
    $smarty->clearConfig('foobar');
    ?>

       

See also [`getConfigVars()`](../../programmers/api-functions/api-get-config-vars.md),
[`config variables`](../../designers/language-variables/language-config-variables.md),
[`config files`](../../designers/config-files.md),
[`{config_load}`](../../designers/language-builtin-functions/language-function-config-load.md),
[`configLoad()`](../../programmers/api-functions/api-config-load.md) and
[`clearAssign()`](../../programmers/api-functions/api-clear-assign.md).
