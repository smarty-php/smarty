\$use\_only\_compiled {#variable.use.only.compiled}
================

If set to TRUE, Smarty will use only compiled templates, alsoù
ignoring the (in)existence of base templates. Compiled filenames
will be constant and relative to the template basename.
Useful to use pre-compiled templates and distribute them already
compiled on production websites. Overrides and disables
[`$merge_compiled_includes`](#variable.merge.compiled.includes)
