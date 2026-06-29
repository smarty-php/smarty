getConfigVars()

returns the given loaded config variable value

Description
===========

array

getConfigVars

string

varname

If no parameter is given, an array of all loaded 
[config variables](../../designers/language-variables/language-config-variables.md) is returned.


    <?php

    // get loaded config template var #foo#
    $myVar = $smarty->getConfigVars('foo');

    // get all loaded config template vars
    $all_config_vars = $smarty->getConfigVars();

    // take a look at them
    print_r($all_config_vars);
    ?>

       

See also [`clearConfig()`](../../programmers/api-functions/api-clear-config.md),
[`{config_load}`](../../designers/language-builtin-functions/language-function-config-load.md),
[`configLoad()`](../../programmers/api-functions/api-config-load.md) and
[`getTemplateVars()`](../../programmers/api-functions/api-get-template-vars.md).
