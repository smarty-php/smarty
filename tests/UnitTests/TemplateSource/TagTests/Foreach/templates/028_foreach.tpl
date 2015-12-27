{strip}
{$x = 'hallo'}
{foreach from=$foo item=x key=y name='b'}
    outer={$smarty.foreach.b.index}#{$y}-{$x}
{foreach from=$bar item=x name='b'}
    inner={$smarty.foreach.b.iteration}#{$y}-{$x}
{/foreach}##
{/foreach}
###{$x}