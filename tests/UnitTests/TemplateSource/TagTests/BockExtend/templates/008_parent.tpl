test:{$test nocache} compiled:# rendered:{$test}
{block name='b'}(parent|b)content {$parent} b(parent|/b){/block}