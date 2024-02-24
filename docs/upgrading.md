# Upgrading from an older version

## Upgrading from v4 to v5

Smarty 5 adds support for PHP8.2 and drops support for PHP7.1. Smarty also adds support for new features
such as the ternary operator (`{$test ? $a : $b}` and `{$var ?: $value_if_falsy}`), the null coalescing operator (`{$var ?? $value_if_null}`) 
and positional parameters for custom tags.
Smarty 5 also has a brand-new extension architecture that allows you to write neat extension packs.

### Namespaces
All Smarty code has been moved into the `\Smarty` namespace. This reduces
the chance of conflicts with other (your) code.

For simple use-cases, you only need to add `use Smarty\Smarty;` to your script and everything will work. 

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty();

$smarty->display('homepage.tpl');
```
For more information, see [getting started](getting-started.md).

If you extend Smarty or use Smarty plug-ins, please review your code to see if they assume specific class or method names.
E.g.: `Smarty_Internal_Template` is now `\Smarty\Template\`, `SmartyException` is now `\Smarty\Exception`.

### Variable scope bubbling
Template variable scope bubbling has been simplified and made more consistent. The global scope now equals the Smarty 
scope in order to avoid global state side effects. Please read the [documentation on language variable scope](designers/language-variables/language-variable-scopes.md)
for more details.

Also, `{config_load}` no longer has a `scope` parameter, which means you can no longer load config
from inside your template into the global scope (again, to avoid global state side effects). If you 
need to set global config, use the [configLoad API method](api/variables/config-files.md) from your PHP code.

### Using native PHP-functions or userland functions in your templates
You can no longer use native PHP-functions or userland functions in your templates without registering them. 
If you need a function in your templates, register it first.

The easiest way to do so is as follows:
```php
// native PHP functions used as modifiers need to be registered
$smarty->registerPlugin('modifier', 'substr', 'substr');

// userland PHP functions used as modifiers need to be registered
$smarty->registerPlugin('modifier', 'my_special_func', 'my_special_func');
```

But you may want to consider writing a proper [extension](api/extending/extensions.md).

### Removed undocumented tags

Smarty 4 still supported some tags that have been carried over from previous version, but have never been documented.

- `{block_parent}` should be replaced with `{$smarty.block.parent}`
- `{parent}` should be replaced with `{$smarty.block.parent}`
- `{block_child}` should be replaced with `{$smarty.block.child}`
- `{child}` should be replaced with `{$smarty.block.child}`

- `{make_nocache}` is no longer supported
- `{insert}` is no longer supported

### Removed Smarty API properties

In Smarty 4, you could make many configuration changes by directly accessing public properties on the Smarty object in PHP.
In many cases, these properties have been made private, and you should now use the appropriate setter method:

- `$smarty->left_delimiter` should be replaced with `$smarty->getLeftDelimiter()`/`$smarty->setLeftDelimiter()`
- `$smarty->right_delimiter` should be replaced with `$smarty->getRightDelimiter()`/`$smarty->setRightDelimiter()`
- `$smarty->autoload_filters` should be replaced with `$smarty->registerFilter()`
- `$smarty->template_dir` should be replaced with `$smarty->setTemplateDir()`
- `$smarty->cache_dir` should be replaced with `$smarty->setCacheDir()`
- `$smarty->compile_dir` should be replaced with `$smarty->setCompileDir()`

Other public properties have been removed altogether, and you should no longer access them:

- `$smarty->_current_file`
- `$smarty->allow_ambiguous_resources` (ambiguous resources handlers should still work)
- `$smarty->registered_filters`
- `$smarty->direct_access_security`
- `$smarty->trusted_dir`
- `$smarty->allow_php_templates`
- `$smarty->php_functions`
- `$smarty->php_modifiers`

### Backwards incompatible changes to custom plugins

We have dropped support for `$smarty->plugins_dir` and `$smarty->use_include_path`.
Use `$smarty->addPluginsDir()` or consider writing a proper [extension](api/extending/extensions.md).

The 'insert' plugin type is no longer supported.

The `$cache_attrs` parameter for registered plugins is no longer supported.

### Removed Smarty API methods

Search your code for the following changes:

- `$smarty->getTags()` is no longer supported
- `$smarty->appendByRef()` should be replaced with `$smarty->append()`
- `$smarty->assignByRef()` should be replaced with `$smarty->assign()`
- `$smarty->loadPlugin()` should be replaced with `$smarty->registerPlugin()`

### Removed PHP constants

The following constants have been removed to prevent global side effects.

- `SMARTY_DIR`
- `SMARTY_SYSPLUGINS_DIR`
- `SMARTY_PLUGINS_DIR`
- `SMARTY_MBSTRING`
- `SMARTY_HELPER_FUNCTIONS_LOADED`

### Other changes

- Smarty now always runs in multibyte mode. Make sure you use the [PHP multibyte extension](https://www.php.net/manual/en/book.mbstring.php) in production for optimal performance.
- Generated `<script>` tags lo longer have deprecated `type="text/javascript"` or `language="Javascript"` attributes
- Smarty will throw a compiler exception instead of silently ignoring a modifier on a function call, like this: `{include|dot:"x-template-id" file="included.dot.tpl"}`
- The ::getFile() method of a CompilerException will now return the full path of the template being compiled, if possible. This used to be 'file:relative_dir/filename.tpl'.

## Upgrading from v3 to v4

Smarty 4 is mostly identical to Smarty 3. Most notably, it adds support for PHP8 and drops support for PHP7.0 and below.
Additionally, some deprecated features that have long been discouraged have been dropped from the language.

### Muting PHP8 warnings
If you simultaneously upgrade Smarty to v4 van PHP to v8, you may notice your error logs filling up with warnings about undefined or null template vars 
due to a change in how PHP handles these. This may be helpful to spot errors, but if you find this annoying, you can use
`$smarty->muteUndefinedOrNullWarnings()` to make Smarty convert these warnings into notices.

### ASP tags
You can no longer user ASP-style tags like `<% %>` and `<%= %>` in your templates.
Replace them with `{...}` tags.

### SmartyBC
Check your codebase for `SmartyBC`.
We have dropped deprecated API calls that where only accessible through the SmartyBC class.

### No more embedded PHP
We have completely dropped support for `{php}` and `{include_php}` tags and embedded PHP in templates.
Check your templates for this, and rewrite any embedded PHP blocks, by moving logic to your PHP files or by
creating a [custom tag](./api/extending/tags.md). 

### Other changes

Search your code for the following changes: 

- `SMARTY_RESOURCE_CHAR_SET` and `SMARTY_RESOURCE_DATE_FORMAT` constants have been removed
- `Smarty::muteExpectedErrors` and `Smarty::unmuteExpectedErrors` API methods have been removed
- `Smarty::getVariable` method has been removed. Use [Smarty::getTemplateVars](designers/language-builtin-functions/language-function-assign.md) instead.
- [Smarty::registerResource](api/resources.md) no longer accepts an array of callback functions





