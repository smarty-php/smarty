# Custom modifiers

Modifiers are little functions that are applied
to a variable in the template before it is displayed or used in some
other context. Smarty comes with a bunch of [modifiers](../../designers/language-modifiers/index.md), but you can 
easily add your own.

In order to do so, you must write a function that accepts as its first parameter the value on which the
modifier is to operate. The rest of the parameters are optional, depending on what kind of operation is to be performed.

The modifier has to return the result of its processing.

For example:
```php
<?php

function smarty_modifier_substr($string, $offset, $length) {
    return substr($string, $offset, $length);
}

$smarty->registerPlugin(Smarty\Smarty::PLUGIN_MODIFIER, 'substr', 'smarty_modifier_substr');
```

You can now use this in your templates as follows:
```smarty
{$applicationName|substr:0:20}
```
