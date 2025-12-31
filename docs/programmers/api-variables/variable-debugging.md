\$debugging {#variable.debugging}
===========

This enables the [debugging console](#chapter.debugging.console). The
console is a JavaScript popup window that informs you of the
[included](../../designers/language-builtin-functions/language-function-include.md) templates, variables
[assigned](../../programmers/api-functions/api-assign.md) from php and 
[config file variables](../../designers/language-variables/language-config-variables.md) for the current script. It does
not show variables assigned within a template with the
[`{assign}`](../../designers/language-builtin-functions/language-function-assign.md) function.

The console can also be enabled from the url with
[`$debugging_ctrl`](variable-debugging-ctrl.md).

See also [`{debug}`](../../designers/language-custom-functions/language-function-debug.md),
[`$debug_tpl`](variable-debug-template.md), and
[`$debugging_ctrl`](variable-debugging-ctrl.md).
