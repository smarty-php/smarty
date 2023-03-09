# Template Inheritance

Inheritance brings the concept of Object Oriented Programming to
templates, allowing you to define one (or more) base templates that can
be extended by child templates. Extending means that the child template
can override all or some of the parent named block areas.

-   The inheritance tree can be as deep as you want, meaning you can
    extend a file that extends another one that extends another one and
    so on.

-   The child templates can not define any content besides what's
    inside [`{block}`](../../designers/language-builtin-functions/language-function-block.md) tags they override.
    Anything outside of [`{block}`](../../designers/language-builtin-functions/language-function-block.md) tags will
    be removed.

-   The content of [`{block}`](../../designers/language-builtin-functions/language-function-block.md) tags from child
    and parent templates can be merged by the `append` or `prepend`
    [`{block}`](../../designers/language-builtin-functions/language-function-block.md) tag option flags and
    `{$smarty.block.parent}` or `{$smarty.block.child}` placeholders.

-   Template inheritance is a compile time process which creates a
    single compiled template file. Compared to corresponding solutions
    based on subtemplates included with the
    [`{include}`](../../designers/language-builtin-functions/language-function-include.md) tag it does have much
    better performance when rendering.

-   The child template extends its parent defined with the
    [`{extends}`](../../designers/language-builtin-functions/language-function-extends.md) tag, which must be the
    first line in the child template. Instead of using the
    [`{extends}`](../../designers/language-builtin-functions/language-function-extends.md) tags in the template files
    you can define the whole template inheritance tree in the PHP script
    when you are calling [`fetch()`](#api.fetch) or
    [`display()`](#api.display) with the `extends:` template resource
    type. The later provides even more flexibility.

> **Note**
>
> When `$compile_check` is enabled, all files in the inheritance tree
> are checked for modifications upon each invocation. You may want to
> disable `$compile_check` on production servers for this reason.

> **Note**
>
> If you have a subtemplate which is included with
> [`{include}`](../../designers/language-builtin-functions/language-function-include.md) and it contains
> [`{block}`](../../designers/language-builtin-functions/language-function-block.md) areas it works only if the
> [`{include}`](../../designers/language-builtin-functions/language-function-include.md) itself is called from within
> a surrounding [`{block}`](../../designers/language-builtin-functions/language-function-block.md). In the final
> parent template you may need a dummy
> [`{block}`](../../designers/language-builtin-functions/language-function-block.md) for it.

layout.tpl (parent)

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
      

To render the above use

```php
<?php
$smarty->display('mypage.tpl');
```

The resulting output is

```smarty
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

Instead of using [`{extends}`](../../designers/language-builtin-functions/language-function-extends.md) tags in the
template files you can define the inheritance tree in your PHP script by
using the [`extends:` resource](../resources/resources-extends.md) type.

The code below will return same result as the example above.

```php
<?php
$smarty->display('extends:layout.tpl|myproject.tpl|mypage.tpl'); 
```

See also [`{block}`](../../designers/language-builtin-functions/language-function-block.md),
[`{extends}`](../../designers/language-builtin-functions/language-function-extends.md) and [`extends:`
resource](../resources/resources-extends.md)
