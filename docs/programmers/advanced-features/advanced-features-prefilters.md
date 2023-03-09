# Prefilters

Template prefilters are PHP functions that your templates are ran
through *before they are compiled*. This is good for preprocessing your
templates to remove unwanted comments, keeping an eye on what people are
putting in their templates, etc.

Prefilters can be
[registered](../api-functions/api-register-filter.md) or added as part of a [custom extension](extending-smarty.md).

Smarty will pass the template source code as the first argument, and
expect the function to return the resulting template source code.

This will remove all the html comments in the template source.

```php
<?php
// put this in your application
function remove_dw_comments($tpl_source, \Smarty\Template\ $template)
{
    return preg_replace("/<!--#.*-->/U",'',$tpl_source);
}

// register the prefilter
$smarty->registerFilter('pre','remove_dw_comments');
$smarty->display('index.tpl');
```

See also [`registerFilter()`](../api-functions/api-register-filter.md),
[postfilters](advanced-features-postfilters.md), and
[`addExtension()`](../api-functions/add-extension.md).
