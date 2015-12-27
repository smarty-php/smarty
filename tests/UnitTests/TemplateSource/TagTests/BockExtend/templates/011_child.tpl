{extends file='011_parent.tpl'}
{strip}
{block name='b'}(child|b)content
    {block name='c'}(child|c)content
        {block name='n'}(child|n)content
            {include file='011_include.tpl'}
            n(child|/n)
        {/block}
        c(child|/c)
    {/block}
    b(child|/b)
{/block}
