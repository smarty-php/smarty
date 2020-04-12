{strip}
{for $i=20;$i<22;$i++}
{foreach ['a','b'] as $a}
{foreach [1,2,3,4,5] as $i}
    {if $i == 3}{continue}{/if}
    {$a}{$i}
{/foreach}
{/foreach}
for{$i}{/for}