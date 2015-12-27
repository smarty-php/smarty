{extends file='021_child.tpl'}
{block name='b' prepend}(grand|b)content {$grand} b(grand|/b){/block}