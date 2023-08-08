# Loading data from config files

Instead of [assigning data to templates from PHP](assigning.md), you can also
use a config file.

## Example config file
Config files are best suited to manage template settings
from one file. One example is a multi-language application.
Instead of writing multiple templates to support different languages, 
you can write a single template file and load your language dependent strings
from config files.

Example `lang.en.ini`:
```ini
# global variables
pageTitle = "Main Menu"

[Customer]
pageTitle = "Customer Info"

[Login]
pageTitle = "Login"
focus = "username"
Intro = """This is a value that spans more
           than one line. you must enclose
           it in triple quotes."""

```

Values of [config file variables](../../designers/language-variables/language-config-variables.md) can be in
quotes, but not necessary. You can use either single or double quotes.
If you have a value that spans more than one line, enclose the entire
value with triple quotes \("""\). You can put comments into config
files by any syntax that is not a valid config file syntax. We recommend
using a `#` (hash) at the beginning of the line.

The example config file above has two sections. Section names are
enclosed in \[brackets\]. Section names can be arbitrary strings not
containing `[` or `]` symbols. The variable at the top is a global
variable. Global variables are always
loaded from the config file. If a particular section is loaded, then the
global variables and the variables from that section are also loaded. If
a variable exists both as a global and in a section, the section
variable is used.

## Loading a config file

Config files are loaded into templates with the built-in template
function [`{config_load}`](../../designers/language-builtin-functions/language-function-config-load.md) or by calling
`configLoad()` from PHP:

```php
<?php
$smarty->configLoad('lang.en.ini');
```

Load a specific section with:

```php
<?php
$smarty->configLoad('lang.en.ini', 'Customer');
```

Note that the global section will always be loaded.

## Retrieving config variables in PHP


## Loading from a resource
Config files (or resources) are loaded by the same resource facilities
as templates. That means that a config file can also be loaded from a db. See [resources](../resources.md)
for more information.

## Config overwrite
If you name two variables the same within a section,
the last one will be used unless you call:
```php
<?php
$smarty->setConfigOverwrite(false);
```
When config overwrite is disabled, Smarty will create arrays of config file variables when it encounters
multiple entries with the same name.

See also [`{config_load}`](../../designers/language-builtin-functions/language-function-config-load.md),
[`$default_config_handler_func`](../../programmers/api-variables/variable-default-config-handler-func.md),
[`getConfigVars()`](../../programmers/api-functions/api-get-config-vars.md),
[`clearConfig()`](../../programmers/api-functions/api-clear-config.md) and
[`configLoad()`](../../programmers/api-functions/api-config-load.md)
