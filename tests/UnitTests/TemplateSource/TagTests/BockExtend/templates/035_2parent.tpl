test:{$test nocache} compiled:# rendered:{$test}
{block name='b'}(parent|b)content {child assign='foo'}{$foo} b(parent|/b){/block}