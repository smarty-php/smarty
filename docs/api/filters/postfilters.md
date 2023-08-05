# Postfilters

Template postfilters are PHP functions that your templates are ran
through *after they are compiled*.

Smarty will
pass the compiled template code as the first argument, and expect the
function to return the result of the processing, which must also be valid PHP code.

Prefilters can be either added as part of an [Extension](../extending/extensions.md) or
registered as shown below.


```php
<?php

function add_header_comment($tpl_source, \Smarty\Template\ $template)
{
    return "<?php echo \"<!-- Created by Smarty! -->\n\"; ?>\n".$tpl_source;
}

// register the postfilter
$smarty->registerFilter('post', 'add_header_comment');
$smarty->display('index.tpl');
```

The postfilter above will make the compiled Smarty template `index.tpl`
look like:

```smarty
<!-- Created by Smarty! -->
{* rest of template content... *}
```
