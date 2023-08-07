# Template Inheritance

Inheritance allows you to define base templates that can
be extended by child templates. Extending means that the child template
can override all or some of the named block areas in the base template.

When you render the child template, the result will as if you rendered
the base template, with only the block(s) that you have overridden in the 
child templates differing.

-   The inheritance tree can be as deep as you want, meaning you can
    extend a file that extends another one that extends another one and
    so on.

-   The child templates can not define any content besides what's
    inside [`{block}`](../designers/language-builtin-functions/language-function-block.md) tags they override.
    Anything outside of [`{block}`](../designers/language-builtin-functions/language-function-block.md) tags will
    be removed.

-   Template inheritance is a compile time process which creates a
    single compiled template file. Compared to corresponding solutions
    based on subtemplates included with the
    [`{include}`](../designers/language-builtin-functions/language-function-include.md) tag it does have much
    better performance when rendering.

## Basic inheritance

First, create a base template with one or more [blocks](../designers/language-builtin-functions/language-function-block.md). 
Then, create a child template. The child template 
must have an [{extends} tag](../designers/language-builtin-functions/language-function-extends.md) on its first line.

The child template can redefine one or more blocks defined in the base template.

See below for a simple example.

layout.tpl (base)

```smarty
<html>
    <head>
      <title>{block name=title}Default Page Title{/block}</title>
      {block name=head}{/block}
    </head>
    <body>
        {block name=body}{/block}
    </body>
</html>
```


myproject.tpl (child)

```smarty
{extends file='layout.tpl'}
{block name=head}
  <link href="/css/mypage.css" rel="stylesheet" type="text/css"/>
  <script src="/js/mypage.js"></script>
{/block}
```

mypage.tpl (grandchild)

```smarty
{extends file='myproject.tpl'}
{block name=title}My Page Title{/block}
{block name=head}
  <link href="/css/mypage.css" rel="stylesheet" type="text/css"/>
  <script src="/js/mypage.js"></script>
{/block}
{block name=body}My HTML Page Body goes here{/block}
```


To render the above, you would use:

```php
<?php
$smarty->display('mypage.tpl');
```

The resulting output is:

```html
<html>
    <head>
      <title>My Page Title</title>
      <link href="/css/mypage.css" rel="stylesheet" type="text/css"/>
      <script src="/js/mypage.js"></script>
    </head>
    <body>
     My HTML Page Body goes here
    </body>
</html>
```

> **Note**
>
> When [compile-check](./configuring.md#disabling-compile-check) is enabled, all files 
> in the inheritance tree
> are checked for modifications upon each invocation. You may want to
> disable compile-check on production servers for this reason.

> **Note**
>
> If you have a subtemplate which is included with
> [`{include}`](../designers/language-builtin-functions/language-function-include.md) and it contains
> [`{block}`](../designers/language-builtin-functions/language-function-block.md) areas it works only if the
> [`{include}`](../designers/language-builtin-functions/language-function-include.md) itself is called from within
> a surrounding [`{block}`](../designers/language-builtin-functions/language-function-block.md). In the final
> parent template you may need a dummy
> [`{block}`](../designers/language-builtin-functions/language-function-block.md) for it.


## Using append and prepend
The content of [`{block}`](../designers/language-builtin-functions/language-function-block.md) tags from child
and parent templates can be merged by the `append` or `prepend`
[`{block}`](../designers/language-builtin-functions/language-function-block.md) tag option flags and
`{$smarty.block.parent}` or `{$smarty.block.child}` placeholders.

## Extends resource type
Instead of using [`{extends}`](../designers/language-builtin-functions/language-function-extends.md) tags in the
template files you can define the inheritance tree in your PHP script by
using the [`extends:` resource](resources.md#the-extends-resource) type.

The code below will return same result as the example above.

```php
<?php
$smarty->display('extends:layout.tpl|myproject.tpl|mypage.tpl'); 
```
