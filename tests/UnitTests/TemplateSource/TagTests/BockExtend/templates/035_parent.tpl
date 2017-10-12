test:{$test nocache} compiled:# rendered:{$test}
{block name='b'}(parent|b)content {$foo=$smarty.block.child}{$foo} b(parent|/b){/block}