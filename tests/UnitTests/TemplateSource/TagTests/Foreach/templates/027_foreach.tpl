{strip}
{$x = "hallo"}
{foreach $foo as $x}
    outer={$x@index}#{$x}
{foreach $bar as $x}
    inner={$x@index}#{$x}
{/foreach}##
{/foreach}
###{$x}