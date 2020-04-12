test:{$test nocache} compiled:# rendered:{$test}
{block name='b'}(parent|b)content {block_child} b(parent|/b){/block}