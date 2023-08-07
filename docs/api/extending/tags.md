# Custom tags

You can add your own tags to the Smarty language. 

## Runtime tags

Usually, you'll add a runtime tag. Adding a runtime tag requires you to provide a callback function that accepts
two parameters:

- `$params`:  all attributes from the template as an associative array.
- `$template`: a `Smarty\Template` object representing the template where tag was used.

The output (return value) of the function will be substituted in place
of the tag in the template.

If the function needs to assign some variables to the template or use
some other Smarty-provided functionality, it can use the supplied
`$template` object to do so.

```php
<?php

function smarty_tag_eightball($params, \Smarty\Template $template): string {
    $answers = [
        'Yes',
        'No',
        'No way',
        'Outlook not so good',
        'Ask again soon',
        'Maybe in your reality'
    ];

    $result = array_rand($answers);
    return $answers[$result];
}

$smarty->registerPlugin(Smarty\Smarty::PLUGIN_FUNCTION, 'eightball', 'smarty_tag_eightball');
```

Which can now be used in the template as:

```smarty
Question: Will we ever have time travel?
Answer: {eightball}.
```

## Compiler tags

Compiler tags are called only during compilation of the template.

They are useful for injecting PHP code or time-sensitive static content
into the template. If there is both a compiler function and a runtime tag registered under the same name,
the compiler function has precedence.

The compiler function is passed two parameters: the params array which
contains precompiled strings for the attribute values and the Smarty
object. It's supposed to return the code to be injected into the
compiled template including the surrounding PHP tags.

Example:
```php
<?php

function smarty_compiler_tplheader($params, Smarty $smarty) {
    return "<?php\necho '" . $smarty->_current_file . " compiled at " . date('Y-m-d H:M'). "';\n?>";
}

$smarty->registerPlugin(Smarty\Smarty::PLUGIN_COMPILER, 'tplheader', 'smarty_compiler_tplheader');
```

This function can be called from the template as:

```smarty
{* this function gets executed at compile time only *}
{tplheader}
```

The resulting PHP code in the compiled template would be something like
this:

```php
<?php
echo 'index.tpl compiled at 2023-02-20 20:02';
```
