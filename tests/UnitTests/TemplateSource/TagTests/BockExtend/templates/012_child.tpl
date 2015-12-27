{extends file='012_parent.tpl'}
{block name='c' prepend}(child|c)content {$child} c(child|/c){/block}