Debugging Console {#chapter.debugging.console}
=================

There is a debugging console included with Smarty. The console informs
you of all the [included](#language.function.include) templates,
[assigned](#api.assign) variables and
[config](#language.config.variables) file variables for the current
invocation of the template. A template file named `debug.tpl` is
included with the distribution of Smarty which controls the formatting
of the console.

Set [`$debugging`](#variable.debugging) to TRUE in Smarty, and if needed
set [`$debug_tpl`](#variable.debug_template) to the template resource
path to `debug.tpl` (this is in [`SMARTY_DIR`](#constant.smarty.dir) by
default). When you load the page, a Javascript console window will pop
up and give you the names of all the included templates and assigned
variables for the current page.

To see the available variables for a particular template, see the
[`{debug}`](#language.function.debug) template function. To disable the
debugging console, set [`$debugging`](#variable.debugging) to FALSE. You
can also temporarily turn on the debugging console by putting
`SMARTY_DEBUG` in the URL if you enable this option with
[`$debugging_ctrl`](#variable.debugging.ctrl).

> **Note**
>
> The debugging console does not work when you use the
> [`fetch()`](#api.fetch) API, only when using
> [`display()`](#api.display). It is a set of javascript statements
> added to the very bottom of the generated template. If you do not like
> javascript, you can edit the `debug.tpl` template to format the output
> however you like. Debug data is not cached and `debug.tpl` info is not
> included in the output of the debug console.

> **Note**
>
> The load times of each template and config file are in seconds, or
> fractions thereof.

See also [troubleshooting](#troubleshooting).
