{strip}
{foreach [1,2,3,4,5] as $i}
    {if $i == 3}{continue}{/if}
    {$i}
{/foreach}