test:{$test nocache} compiled:# rendered:{$test}
{block 'grand'}{include './include/include.tpl'}{/block}
{block 'child'}{include './include/include.tpl'}{/block}
{block 'parent'}{include './include/include.tpl'}{/block}
