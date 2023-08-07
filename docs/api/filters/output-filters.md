# Output filters

When a template is rendered, its output can be sent through one or more
output filters.

> **Note**
> This differs from [`prefilters`](prefilters.md) and 
> [`postfilters`](postfilters.md) because, pre- and postfilters
> operate on compiled templates before they are saved to the disk, whereas
> output filters operate on the template output when it is executed.

Smarty will pass the template output as the first argument, and expect the function
to return the result of the processing.

Output filters can be either added as part of an [Extension](../extending/extensions.md) or 
registered as shown below.

This will provide a rudimentary protection against spambots:
```php
<?php

function protect_email($tpl_output, \Smarty\Template\ $template)
{
    return preg_replace(
        '!(\S+)@([a-zA-Z0-9\.\-]+\.([a-zA-Z]{2,3}|[0-9]{1,3}))!',
        '$1%40$2', 
        $tpl_output
    );
}

// register the outputfilter
$smarty->registerFilter("output", "protect_email");
$smarty->display("index.tpl');

```
