{extends '030_child.tpl'}
{strip}
{block 'b3'}
   [b3 grandchild b3={$b3} include={include '030_include.tpl'} b3]
{/block}
{block 'b4' nocache}
    [b4-nocache grandchild b4={$b4} b4-nocache]
{/block}
{block 'b5'}
    [b5 grandchild b5={$b5} b5]
{/block}
{block 'b6'}
    [b6 grandchild b6={$b6} b6]
{/block}