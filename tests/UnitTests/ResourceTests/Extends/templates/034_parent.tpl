test:{$test nocache} compiled:# rendered:{$test}
{block 'grandchild'}grandchild - parent{/block}
{block 'child'}child - parent{/block}
{block 'parent'}parent - parent{/block}
