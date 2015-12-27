{extends file='008_parent.tpl'}
{block name='b'}(child|b)content {'escaped <text>'|escape} {$child} {counter start=1} b(child|/b){/block}