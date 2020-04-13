{strip}
{foreach $array as $key => $i}
    {if $key == 1}
        {break}
    {/if}
    {$i}
{/foreach}