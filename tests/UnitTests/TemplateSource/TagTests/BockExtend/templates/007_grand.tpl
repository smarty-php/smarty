{extends file='007_child.tpl'}
{block name='b'}(grand|b)content {$smarty.block.parent} b(grand|/b){/block}