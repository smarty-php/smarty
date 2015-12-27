{extends file='007_parent.tpl'}
{block name='b'}(child|b)content {$smarty.block.parent} b(child|/b){/block}