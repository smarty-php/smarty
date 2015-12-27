{extends file='006_parent.tpl'}
{block name='b'}(child|b)content {$child} b(child|/b){/block}