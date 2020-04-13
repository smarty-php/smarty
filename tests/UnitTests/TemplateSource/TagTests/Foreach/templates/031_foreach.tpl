{strip}
{for $i=20;$i<22;$i++}
{foreach ['a','b'] as $a}
{foreach [1,2,3,4,5] as $i}
    {if $i == 3}{break 2}{/if}
    {$a}{$i}
{/foreach}
{/foreach}
for{$i}{/for}