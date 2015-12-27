test:{$test nocache} compiled:# rendered:{$test}
{block 'b'}(parent|b)content {$parent} b(parent|/b){/block}