{extends file='002_parent.tpl'}
{block name='b'}(child|b)content {$child} b(child|/b){/block}