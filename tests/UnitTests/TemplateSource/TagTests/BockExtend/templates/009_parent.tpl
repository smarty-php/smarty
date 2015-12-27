test:{$test nocache} compiled:# rendered:{$test}
{block name='b'}(parent|b)content {block name='c'}(parent|c)content {$parent} c(parent|/c){/block} b(parent|/b){/block}