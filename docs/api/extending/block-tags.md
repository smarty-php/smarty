# Custom block tags

Block tags are tags of the form: `{func} .. {/func}`. In other
words, they enclose a template block and operate on the contents of this
block. 

Block functions take precedence over normal tags of the same name, that is, you
cannot have both custom tag `{func}` and block tag `{func}..{/func}`.

-   By default, your function implementation is called twice by Smarty:
    once for the opening tag, and once for the closing tag. (See
    `$repeat` below on how to change this.)

-   Only the opening tag of the block has attributes. All attributes are contained in the `$params`
    variable as an associative array. The opening tag attributes are
    also accessible to your function when processing the closing tag.

-   The value of the `$content` variable depends on whether your
    function is called for the opening or closing tag. In case of the
    opening tag, it will be NULL, and in case of the closing tag it will
    be the contents of the template block. Note that the template block
    will have already been processed by Smarty, so all you will receive
    is the template output, not the template source.

-   The parameter `$repeat` is passed by reference to the function
    implementation and provides a possibility for it to control how many
    times the block is displayed. By default `$repeat` is TRUE at the
    first call of the block function (the opening tag) and FALSE on all
    subsequent calls to the block function (the block's closing tag).
    Each time the function implementation returns with `$repeat` being
    TRUE, the contents between `{func}...{/func}` are evaluated and the
    function implementation is called again with the new block contents
    in the parameter `$content`.

Example:
```php
<?php

function smarty_block_translate($params, $content, \Smarty\Template $template, &$repeat) {
    // only output on the closing tag
    if (!$repeat){
        if (isset($content)) {
            $lang = $params['lang'];
            // do some intelligent translation thing here with $content
            return $translation;
        }
    }
}

$smarty->registerPlugin(Smarty\Smarty::PLUGIN_BLOCK, 'translate', 'smarty_block_translate');
```

This can now be used in your templates as follows:

```smarty
{translate lang='nl'}
    Quia omnis nulla omnis iusto est id et.
{/translate}
```
