{extends file='012_child.tpl'}
{block name='b'}(grand|b)content {block name='c'}(grand|c)content c(grand|\c){/block} b(grand|/b){/block}