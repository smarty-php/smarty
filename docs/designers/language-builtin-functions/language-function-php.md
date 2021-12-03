{php} {#language.function.php}
=====

> **Note**
>
> `{php}` tags are deprecated from Smarty, and should not be used. Put
> your PHP logic in PHP scripts or plugin functions instead.

> **Note**
>
> As of Smarty 3.1 the `{php}` tags are only available from
> [SmartyBC](#bc).

The `{php}` tags allow PHP code to be embedded directly into the
template. They will not be escaped, regardless of the
[`$php_handling`](#variable.php.handling) setting.


    {php}
       // including a php script directly from the template.
       include('/path/to/display_weather.php');
    {/php}

      


    {* this template includes a {php} block that assign's the variable $varX *}
    {php}
       global $foo, $bar;
       if($foo == $bar){
         echo 'This will be sent to browser';
       }
      // assign a variable to Smarty
      $this->assign('varX','Toffee');
    {/php}
    {* output the variable *}
    <strong>{$varX}</strong> is my fav ice cream :-)

      

See also [`$php_handling`](#variable.php.handling),
[`{include_php}`](#language.function.include.php),
[`{include}`](#language.function.include),
[`{insert}`](#language.function.insert) and [componentized
templates](#tips.componentized.templates).
