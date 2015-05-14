{strip}
{function 'recursion2' level=0}
{$level}
    {if $level < 5}
        {call name=recursion2 level=$level+1}
    {/if}
{/function}

{recursion2}
{/strip}