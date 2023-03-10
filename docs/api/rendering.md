# Rendering templates

## Fetching or rendering templates directly
As explained in [basics](basics.md), you can use `$smarty->fetch()` or `$smarty->display()`
to render a template directly.

```php
<?php

use Smarty\Smarty;
$smarty = new Smarty();

$smarty->display('homepage.tpl');

// or

$output = $smarty->fetch('homepage.tpl');
```

When you use `display()`, Smarty renders the template to the standard output stream.
`fetch()` returns the output instead of echoing it.

The example above uses simple filenames to load the template. Smarty also supports
[loading templates from resources](resources.md).

## Creating a template object
You can also create a template object which later can be prepared first,
and rendered later. This can be useful, for example if you plan to re-use several 
templates.

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

// create template object with its private variable scope
$tpl = $smarty->createTemplate('index.tpl');

// assign a variable (available only to this template)
$tpl->assign('title', 'My Homepage!');

// display the template
$tpl->display();
```

More on assigning variables in [using data in templates](variables/assigning.md).


## Testing if a template exists
You can use `templateExists()` to check whether a template exists before you attempt to use it.

It accepts either a path to the template on the filesystem or a
resource string specifying the template.

This example uses `$_GET['page']` to
[`{include}`](../designers/language-builtin-functions/language-function-include.md) a content template. If the
template does not exist then an error page is displayed instead. First, 
the `page_container.tpl`

```smarty
<html>
    <head>
        <title>{$title|escape}</title>
    </head>
    <body>
        {* include middle content page *}
        {include file=$content_template}
    </body>
</html>
```

And the php script:

```php
<?php

// set the filename eg index.inc.tpl
$mid_template = $_GET['page'].'.inc.tpl';

if (!$smarty->templateExists($mid_template)){
    $mid_template = 'page_not_found.tpl';
}
$smarty->assign('content_template', $mid_template);

$smarty->display('page_container.tpl');
```
