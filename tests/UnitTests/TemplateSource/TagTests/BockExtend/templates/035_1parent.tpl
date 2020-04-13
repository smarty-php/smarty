test:{$test nocache} compiled:# rendered:{$test}
{block name='b'}(parent|b)content {$smarty.block.child} b(parent|/b){/block}