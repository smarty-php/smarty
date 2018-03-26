{extends file=$parenttpl}
{block name='b'}(child|b)content {$child} b(child|/b){/block}