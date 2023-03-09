# Postfilters

Template postfilters are PHP functions that your templates are ran
through *after they are compiled*. 

Postfilters can be
[registered](../api-functions/api-register-filter.md) or added as part of a [custom extension](extending-smarty.md). Smarty will
pass the compiled template code as the first argument, and expect the
function to return the result of the processing.

```php
<?php
// put this in your application
function add_header_comment($tpl_source, \Smarty\Template\ $template)
{
    return "<?php echo \"<!-- Created by Smarty! -->\n\"; ?>\n".$tpl_source;
}

// register the postfilter
$smarty->registerFilter('post','add_header_comment');
$smarty->display('index.tpl');
```

The postfilter above will make the compiled Smarty template `index.tpl`
look like:

```smarty
<!-- Created by Smarty! -->
{* rest of template content... *}
```

See also [`registerFilter()`](../api-functions/api-register-filter.md),
[prefilters](advanced-features-prefilters.md),
[outputfilters](advanced-features-outputfilters.md), and
[`addExtension()`](../api-functions/add-extension.md).
