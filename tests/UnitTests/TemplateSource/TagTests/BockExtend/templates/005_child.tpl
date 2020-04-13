{extends '005_parent.tpl'}
{block 'b' append}(child|b)content {$child} b(child|/b){/block}