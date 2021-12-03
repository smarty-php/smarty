Extending Smarty With Plugins {#plugins}
=============================

Version 2.0 introduced the plugin architecture that is used for almost
all the customizable functionality of Smarty. This includes:

-   functions

-   modifiers

-   block functions

-   compiler functions

-   prefilters

-   postfilters

-   outputfilters

-   resources

-   inserts

With the exception of resources, backwards compatibility with the old
way of registering handler functions via register\_\* API is preserved.
If you did not use the API but instead modified the class variables
`$custom_funcs`, `$custom_mods`, and other ones directly, then you will
need to adjust your scripts to either use the API or convert your custom
functionality into plugins.

PROGRAMMERS.PLUGINS.PLUGINS-HOWTO
PROGRAMMERS.PLUGINS.PLUGINS-NAMING-CONVENTIONS
PROGRAMMERS.PLUGINS.PLUGINS-WRITING
PROGRAMMERS.PLUGINS.PLUGINS-FUNCTIONS
PROGRAMMERS.PLUGINS.PLUGINS-MODIFIERS
PROGRAMMERS.PLUGINS.PLUGINS-BLOCK-FUNCTIONS
PROGRAMMERS.PLUGINS.PLUGINS-COMPILER-FUNCTIONS
PROGRAMMERS.PLUGINS.PLUGINS-PREFILTERS-POSTFILTERS
PROGRAMMERS.PLUGINS.PLUGINS-OUTPUTFILTERS
PROGRAMMERS.PLUGINS.PLUGINS-RESOURCES
PROGRAMMERS.PLUGINS.PLUGINS-INSERTS
