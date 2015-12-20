{extends '030_parent.tpl'}
{strip}
{block 'b3'}
   [b3 child b3={$b3} b3]
{/block}
{block 'b4'}
    [b4 child b4={$b4} block.child={$smarty.block.child} b4]
{/block}
{block 'b5' nocache}
    [b5-nocache child b5={$b5} block.child={$smarty.block.child} b5-nocache]
{/block}
{block 'b61'}
   [b61 child b6={$b6} include={include '030_include.tpl'} b61]
{/block}
{block 'b62' nocache}
    [b62-nocache child b6={$b6} include={include '030_include.tpl'} b62-nocache]
{/block}
{block 'b63'}
    [b63 child b6={$b6} include_2={include '030_include_2.tpl'} b63]
{/block}
{block 'b64'}
    [b64 child b6={$b6} include-nocache={include '030_include.tpl' nocache} b64]
{/block}
