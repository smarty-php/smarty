{extends file='004_parent.tpl'}
{block name='b' append}(child|b)content {$child} b(child|/b){/block}