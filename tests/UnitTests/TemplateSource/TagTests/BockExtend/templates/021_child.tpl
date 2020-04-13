{extends file='021_parent.tpl'}
{block name='b' prepend}(child|b)content {$child} b(child|/b){/block}
