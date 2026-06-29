configLoad()

loads config file data and assigns it to the template

Description
===========

void

configLoad

string

file

string

section

This loads [config file](../../designers/config-files.md) data and assigns it to the
template. This works identically to the template
[`{config_load}`](../../designers/language-builtin-functions/language-function-config-load.md) function.

> **Note**
>
> As of Smarty 2.4.0, assigned template variables are kept across
> invocations of [`fetch()`](../../programmers/api-functions/api-fetch.md) and
> [`display()`](../../programmers/api-functions/api-display.md). Config vars loaded from `configLoad()`
> are always global in scope. Config files are also compiled for faster
> execution, and respect the [`$force_compile`](../api-variables/variable-force-compile.md)
> and [`$compile_check`](#variable.compile.check) settings.


    <?php
    // load config variables and assign them
    $smarty->configLoad('my.conf');

    // load a section
    $smarty->configLoad('my.conf', 'foobar');
    ?>

       

See also [`{config_load}`](../../designers/language-builtin-functions/language-function-config-load.md),
[`getConfigVars()`](../../programmers/api-functions/api-get-config-vars.md),
[`clearConfig()`](../../programmers/api-functions/api-clear-config.md) and
[`config variables`](../../designers/language-variables/language-config-variables.md)
