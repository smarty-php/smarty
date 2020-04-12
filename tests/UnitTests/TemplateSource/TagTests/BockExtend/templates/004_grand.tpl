{extends file='004_child.tpl'}
{block name='b' append}(grand|b)content {$grand} b(grand|/b){/block}