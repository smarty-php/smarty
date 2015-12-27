{extends file="018_parent.tpl"}
{strip}
{block name='b'}(child|b)content
    {block name='c'}(child|c)content child c(child|/c)
    {/block}
    {block name='d' hide}(child|d)content {$smarty.block.child} d(child|/d)
    {/block}
    b(child|/b)
{/block}