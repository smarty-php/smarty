# Configuring Smarty

## Setting the template path
By default, Smarty looks for templates to render in `./templates`.

You can change this, or even use multiple paths to use when looking for templates.

If you need to change this, you can use `setTemplateDir()` or `addTemplateDir()`.
Use `getTemplateDir()` to retrieve the configured paths.

```php
<?php

 // set a single directory where the config files are stored
$smarty->setTemplateDir('./templates');

// set multiple directories where templates are stored
$smarty->setTemplateDir(['./templates', './templates_2', './templates_3']);

// add directory where templates files are stored to the current list of dirs
$smarty->addTemplateDir('./templates_1');

// add multiple directories to the current list of dirs
$smarty->addTemplateDir([
    './templates_2',
    './templates_3',
]);

// chaining of method calls
$smarty->setTemplateDir('./templates')
       ->addTemplateDir('./templates_1')
       ->addTemplateDir('./templates_2');

// insert a template dir before exising template dirs
$smarty->prependTemplateDir('./more_important_templates')

// get all directories where config files are stored
$template_dirs = $smarty->getTemplateDir();
var_dump($template_dirs); // array

// get directory identified by key
$template_dir = $smarty->getTemplateDir(0);
var_dump($template_dir); // string
```

## Setting the path for compiled templates
Smarty compiles templates to native PHP to be as fast as possible.
The default path where these PHP-files are stored is `./templates_c`.

If you need to change this, you can use `setCompileDir()`.
Use `getCompileDir()` to retrieve the configured path.

```php
<?php

// set another path to store compiled templates
$smarty->setCompileDir('/data/compiled_templates');

// get directory where compiled templates are stored
$compileDir = $smarty->getCompileDir();
```


## Setting the config path
Smarty can [load data from config files](./variables/config-files.md).
By default, Smarty loads the config files from `./configs`.

You can change this, or even use multiple paths to use when looking for config files.

If you need to change this, you can use `setConfigDir()` or `addConfigDir()`.
Use `getConfigDir()` to retrieve the configured paths.

```php
<?php

 // set a single directory where the config files are stored
$smarty->setConfigDir('./config');

// set multiple directories where config files are stored
$smarty->setConfigDir(['./config', './config_2', './config_3']);

// add directory where config files are stored to the current list of dirs
$smarty->addConfigDir('./config_1');

// add multiple directories to the current list of dirs
$smarty->addConfigDir([
    './config_2',
    './config_3',
]);

// chaining of method calls
$smarty->setConfigDir('./config')
       ->addConfigDir('./config_1', 'one')
       ->addConfigDir('./config_2', 'two');

// get all directories where config files are stored
$config_dirs = $smarty->getConfigDir();
var_dump($config_dirs); // array

// get directory identified by key
$config_dir = $smarty->getConfigDir(0);
var_dump($config_dir); // string
```

## Setting the path for caches
Even though Smarty runs templates as native PHP for maximum speed, it still needs to 
execute the PHP code on each call. If your data doesn't change all that often, you
may be able to speed up your application even more by using output caching.

Output caching can be a tricky subject, so we devoted an entire [section to caching](./caching/basics.md).
Be sure to read that if you want to use caching.

By default, Smarty stores caches to PHP-files in a subdirectory named `./cache`.

If you need to change this, you can use `setCacheDir()`.
Use `getCacheDir()` to retrieve the configured path.

```php
<?php

// set another path to store caches
$smarty->setCacheDir('/data/caches');

// get directory where cached templates are stored
$cacheDir = $smarty->getCacheDir();
```

## Enabling auto-escaping
By default, Smarty does not escape anything you render in your templates. If you use
Smarty to render a HTML-page, this means that you will have to make sure that you do
not render any characters that have a special meaning in HTML, such as `&`, `<` and `>`,
or apply the [escape modifier](../designers/language-modifiers/language-modifier-escape.md) 
to anything you want to render.

If you forget to do so, you may break your HTML page, or even create a vulnerability for 
attacks known as [XSS or Cross Site Scripting](https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html).

Luckily, you can tell Smarty to automatically apply the escape modifier to any dynamic part of your template.
It's like Smarty magically adds `|escape` to every variable you use on a web page.

Enable auto-escaping for HTML as follows:
```php
$smarty->setEscapeHtml(true);
```

When auto-escaping is enabled, the `|escape` modifier's default mode (`html`) has no effect,
to avoid double-escaping. It is possible to force it with the `force` mode.
Other modes (`htmlall`, `url`, `urlpathinfo`, `quotes`, `javascript`) may be used
with the result you might expect, without double-escaping.

Even when auto-escaping is enabled, you might want to display the content of a variable without
escaping it. To do so, use the `|raw` modifier.

Examples (with auto-escaping enabled):
```smarty
{* these three statements are identical *}
{$myVar}
{$myVar|escape}
{$myVar|escape:'html'}

{* no double-escaping on these statements *}
{$var|escape:'htmlall'}
{$myVar|escape:'url'}
{$myVar|escape:'urlpathinfo'}
{$myVar|escape:'quotes'}
{$myVar|escape:'javascript'}

{* no escaping at all *}
{$myVar|raw}

{* force double-escaping *}
{$myVar|escape:'force'}
```

## Disabling compile check
By default, Smarty tests to see if the
current template has changed since the last time
it was compiled. If it has changed, it recompiles that template. 

Once an application is put into production, this compile-check step 
is usually no longer needed and the extra checks can significantly hurt performance. 
Be sure to disable compile checking on production for maximum performance. 
```php
<?php
$smarty->setCompileCheck(\Smarty\Smarty::COMPILECHECK_OFF);
```

If [`caching`](./caching/basics.md) is enabled and compile-check is
enabled, then the cache files will get regenerated if an involved
template file or config file was updated.

## Charset encoding

There are a variety of encodings for textual data, ISO-8859-1 (Latin1)
and UTF-8 being the most popular. Unless you change `\Smarty\Smarty::$_CHARSET`,
Smarty recognizes `UTF-8` as the internal charset.

> **Note**
>
> `ISO-8859-1` has been PHP\'s default internal charset since the
> beginning. Unicode has been evolving since 1991. Since then, it has
> become the one charset to conquer them all, as it is capable of
> encoding most of the known characters even across different character
> systems (latin, cyrillic, japanese, ...). `UTF-8` is unicode\'s most
> used encoding, as it allows referencing the thousands of character
> with the smallest size overhead possible.
>
> Since unicode and UTF-8 are very widespread nowadays, their use is
> strongly encouraged.

> **Note**
>
> Smarty\'s internals and core plugins are truly UTF-8 compatible since
> Smarty 3.1.

```php
<?php
    
// use japanese character encoding
mb_internal_charset('EUC-JP');

\Smarty\Smarty::$_CHARSET = 'EUC-JP';
$smarty = new \Smarty\Smarty();
```
     
