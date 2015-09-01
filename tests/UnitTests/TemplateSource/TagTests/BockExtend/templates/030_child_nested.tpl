{extends file='030_parent.tpl'}
{block name='content1'}
    {block name='content2'}
        child pre {$smarty.block.child} child post {include file='030_include.tpl' nocache}
    {/block}
{/block} 