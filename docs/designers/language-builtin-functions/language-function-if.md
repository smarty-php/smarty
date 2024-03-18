# {if},{elseif},{else}

`{if}` statements in Smarty have much the same flexibility as PHP
[if](https://www.php.net/if) statements, with a few added features for the
template engine. Every `{if}` must be paired with a matching `{/if}`.
`{else}` and `{elseif}` are also permitted. All [operators](../language-basic-syntax/language-syntax-operators.md) are recognized, such as *==*,
*\|\|*, *or*, *&&*, *and*, etc and you can use modifiers as functions, such as *is_array()*.

## Examples
```smarty
{if $name eq 'Fred'}
    Welcome Sir.
{elseif $name eq 'Wilma'}
    Welcome Ma'am.
{else}
    Welcome, whatever you are.
{/if}

{* an example with "or" logic *}
{if $name eq 'Fred' or $name eq 'Wilma'}
   ...
{/if}

{* same as above *}
{if $name == 'Fred' || $name == 'Wilma'}
   ...
{/if}


{* parenthesis are allowed *}
{if ( $amount < 0 or $amount > 1000 ) and $volume >= #minVolAmt#}
   ...
{/if}


{* you can also embed php function calls *}
{if count($var) gt 0}
   ...
{/if}

{* check for array. *}
{if is_array($foo) }
   .....
{/if}

{* check for not null. *}
{if isset($foo) }
   .....
{/if}


{* test if values are even or odd *}
{if $var is even}
   ...
{/if}
{if $var is odd}
   ...
{/if}
{if $var is not odd}
   ...
{/if}


{* test if var is divisible by 4 *}
{if $var is div by 4}
   ...
{/if}


{*
  test if var is even, grouped by two. i.e.,
  0=even, 1=even, 2=odd, 3=odd, 4=even, 5=even, etc.
*}
{if $var is even by 2}
   ...
{/if}

{* 0=even, 1=even, 2=even, 3=odd, 4=odd, 5=odd, etc. *}
{if $var is even by 3}
   ...
{/if}
 
{if isset($name) && $name == 'Blog'}
     {* do something *}
{elseif $name == $foo}
    {* do something *}
{/if}

{if is_array($foo) && count($foo) > 0}
    {* do a foreach loop *}
{/if}
```
