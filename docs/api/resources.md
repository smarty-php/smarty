# Template resources

## The filesystem resource

So far in our examples, we have used simple filenames or paths when loading a template.

For example, to load a template file called `homepage.tpl`, from the filesystem, you could write:
```php
<?php

use Smarty\Smarty;
$smarty = new Smarty();

$smarty->display('homepage.tpl');
```

The filesystem is the default resource. Templates, however, may come 
from a variety of sources. When you render a template, or
when you include a template from within another template, you supply a
resource type, followed by `:` and the appropriate path and template name.

If a resource is not explicitly given, the default resource type is assumed.
The resource type for the filesystem is `file`, which means that the previous example
can be rewritten as follows:
```php
<?php

use Smarty\Smarty;
$smarty = new Smarty();

$smarty->display('file:homepage.tpl');
```

The file resource pulls templates source files from the directories
specified using `Smarty::setTemplateDir()` (see [Configuring Smarty](configuring.md)).

`setTemplateDir` accepts a single path, but can also ben called with an array of paths. 
In that case, the list of directories is traversed in the order they appear in the array. The
first template found is the one to process.

### Templates from a specific directory

Smarty 3.1 introduced the bracket-syntax for specifying an element from
`Smarty::setTemplateDir()`. This allows websites
employing multiple sets of templates better control over which template
to access.

The bracket-syntax can be used as follows:
```php
<?php

// setup template directories
$smarty->setTemplateDir([
    './templates',            // element: 0, index: 0
    './templates_2',          // element: 1, index: 1
    '10' => 'templates_10',   // element: 2, index: '10'
    'foo' => 'templates_foo', // element: 3, index: 'foo'
]);

/*
  assume the template structure
  ./templates/foo.tpl
  ./templates_2/foo.tpl
  ./templates_2/bar.tpl
  ./templates_10/foo.tpl
  ./templates_10/bar.tpl
  ./templates_foo/foo.tpl
*/

// regular access
$smarty->display('file:foo.tpl'); 
// will load ./templates/foo.tpl

// using numeric index
$smarty->display('file:[1]foo.tpl'); 
// will load ./templates_2/foo.tpl

// using numeric string index
$smarty->display('file:[10]foo.tpl'); 
// will load ./templates_10/foo.tpl

// using string index
$smarty->display('file:[foo]foo.tpl'); 
// will load ./templates_foo/foo.tpl

// using "unknown" numeric index (using element number)
$smarty->display('file:[2]foo.tpl'); 
// will load ./templates_10/foo.tpl
```

And, from within a Smarty template:

```smarty
{include file="file:foo.tpl"}
{* will load ./templates/foo.tpl *}

{include file="file:[1]foo.tpl"}
{* will load ./templates_2/foo.tpl *}

{include file="file:[foo]foo.tpl"}
{* will load ./templates_foo/foo.tpl *}
```

### Using absolute paths

Templates outside the specified template directories 
require the `file:` template resource type, followed by the absolute
path to the template (with leading slash).

```php
<?php
$smarty->display('file:/export/templates/index.tpl');
$smarty->display('file:/path/to/my/templates/menu.tpl');
````

And from within a Smarty template:
```smarty
{include file='file:/usr/local/share/templates/navigation.tpl'}
```

> **Note**
>
> With [`Security`](security.md) enabled, access to
> templates outside of the specified templates directories is
> not allowed unless you whitelist those directories.

### Windows file paths 
If you are running on Windows, file paths usually include a drive
letter (such as `C:`) at the beginning of the pathname. Be sure to use `file:` in
the path to avoid namespace conflicts and get the desired results.
```php
<?php
$smarty->display('file:C:/export/templates/index.tpl');
$smarty->display('file:F:/path/to/my/templates/menu.tpl');
```

And from within Smarty template:
```smarty
{include file='file:D:/usr/local/share/templates/navigation.tpl'}
```

### Handling missing templates
If the file resource cannot find the requested template, it will check if there is
a default template handler to call. By default, there is none, and Smarty will return an error,
but you can register a default template handler calling `Smarty::registerDefaultTemplateHandler`
with any [callable](https://www.php.net/manual/en/language.types.callable.php).

```php
<?php

$smarty->registerDefaultTemplateHandler([$this, 'handleMissingTemplate']);

// ...

public function handleMissingTemplate($type, $name, &$content, &$modified, Smarty $smarty) {
    if (/* ... */) {
        // return corrected filepath
        return "/tmp/some/foobar.tpl";
    } elseif (/* ... */) {
        // return a template directly
        $content = "the template source";
        $modified = time();
        return true;
    } else {
        // tell smarty that we failed
        return false;
    }
}

```

## The string and eval resources

Smarty can render templates from a string by using the `string:` or
`eval:` resource.

-   The `string:` resource behaves much the same as a template file. The
    template source is compiled from a string and stores the compiled
    template code for later reuse. Each unique template string will
    create a new compiled template file. If your template strings are
    accessed frequently, this is a good choice. If you have frequently
    changing template strings (or strings with low reuse value), the
    `eval:` resource may be a better choice, as it doesn\'t save
    compiled templates to disk.

-   The `eval:` resource evaluates the template source every time a page
    is rendered. This is a good choice for strings with low reuse value.
    If the same string is accessed frequently, the `string:` resource
    may be a better choice.

> **Note**
>
> With a `string:` resource type, each unique string generates a
> compiled file. Smarty cannot detect a string that has changed, and
> therefore will generate a new compiled file for each unique string. It
> is important to choose the correct resource so that you do not fill
> your disk space with wasted compiled strings.

```php
<?php
$smarty->assign('foo', 'value');
$template_string = 'display {$foo} here';
$smarty->display('string:' . $template_string); // compiles for later reuse
$smarty->display('eval:' . $template_string); // compiles every time
```
From within a Smarty template:
```smarty
{include file="string:$template_string"} {* compiles for later reuse *}
{include file="eval:$template_string"} {* compiles every time *}
```

Both `string:` and `eval:` resources may be encoded with
[`urlencode()`](https://www.php.net/urlencode) or
[`base64_encode()`](https://www.php.net/urlencode). This is not necessary
for the usual use of `string:` and `eval:`, but is required when using
either of them in conjunction with the [`extends resource`](#the-extends-resource).

```php
 <?php
 $smarty->assign('foo','value');
 $template_string_urlencode = urlencode('display {$foo} here');
 $template_string_base64 = base64_encode('display {$foo} here');
 $smarty->display('eval:urlencode:' . $template_string_urlencode); // will decode string using urldecode()
 $smarty->display('eval:base64:' . $template_string_base64); // will decode string using base64_decode()
```

From within a Smarty template:
```smarty
 {include file="string:urlencode:$template_string_urlencode"} {* will decode string using urldecode() *}
 {include file="eval:base64:$template_string_base64"} {* will decode string using base64_decode() *}
```

## The extends resource

The `extends:` resource is used to define child/parent relationships. For details see section of
[Template inheritance](inheritance.md).

> **Note**
>
> Using the extends resource is usually not necessary. If you have a choice, it is normally more flexible and
> intuitive to handle inheritance chains from within the templates using the [{extends} tag](inheritance.md).

When `string:` and `eval:` templates are used, make sure they are properly url or base64 encoded. 

The templates within an inheritance chain are not compiled separately. Only a single compiled template will be generated. 
(If an `eval:` resource is found within an inheritance chain, its "don't save a compile file" property is superseded by 
the `extends:` resource.)

Example:
```php
<?php
$smarty->display('extends:parent.tpl|child.tpl|grandchild.tpl'); 

// inheritance from multiple template sources
$smarty->display('extends:db:parent.tpl|file:child.tpl|grandchild.tpl|eval:{block name="fooBazVar_"}hello world{/block}'); 
```

## The stream resource

Smarty allow you to use [PHP streams](https://www.php.net/manual/en/function.stream-wrapper-register.php) 
as a template resource. Smarty will first look for a registered template resource. If nothing is
found, it will check if a PHP stream is available.  If a stream is available, Smarty will use it 
to fetch the template.

For example, 
```php
<?php
stream_wrapper_register('myresource', MyResourceStream::class);
$smarty->display('myresource:bar.tpl');
```

Or, from within a template:
```smarty
 {include file="myresource:bar.tpl"}
```

## Adding your own resource type
You can create a class that extends `Smarty\Resource\CustomPlugin` to add your own resource type, 
for example to load template from a database.

For example:
```php
<?php
class HelloWorldResource extends Smarty\Resource\CustomPlugin {

    protected function fetch($name, &$source, &$mtime) {
        $source = '{$x="hello world"}{$x}'; // load your template here based on $name
        $mtime = time();
    }

}

// ..

$smarty->registerResource('helloworld', new HelloWorldResource());
```

If a Resource's templates should not be run through the Smarty
compiler, the Custom Resource may extend `\Smarty\Resource\UncompiledPlugin`.
The Resource Handler must then implement the function
`renderUncompiled(\Smarty\Template $_template)`. `$_template` is
a reference to the current template and contains all assigned variables
which the implementor can access via
`$_template->getSmarty()->getTemplateVars()`. These Resources simply echo
their rendered content to the output stream. The rendered output will be
output-cached if the Smarty instance was configured accordingly. See
`src/Resource/PhpPlugin.php` for an example.

If the Resource's compiled templates should not be cached on disk, the
Custom Resource may extend `\Smarty\Resource\RecompiledPlugin`. These Resources
are compiled every time they are accessed. This may be an expensive
overhead. See `src/Resource/StringEval.php` for an
example.

## Changing the default resource type
The default resource type is `file`. If you want to change it, use `Smarty::setDefaultResourceType`.

The following example will change the default resource type to `mysql`:
```php
<?php
$smarty->setDefaultResourceType('mysql');
```
