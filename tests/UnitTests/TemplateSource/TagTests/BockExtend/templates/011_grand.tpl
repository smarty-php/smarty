{extends file='011_child.tpl'}
{strip}
{block name='c' append}
    (grand|c)content {$grand} c(grand|\c)
{/block}
