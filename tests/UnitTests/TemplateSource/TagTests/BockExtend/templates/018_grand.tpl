{extends file="018_child.tpl"}
{strip}
{block name='c' hide}(grand|c)content {$smarty.block.child} c(grand|/c)
{/block}
{block name='d'}(grand|d)content grand d(grand|/d)
{/block}
