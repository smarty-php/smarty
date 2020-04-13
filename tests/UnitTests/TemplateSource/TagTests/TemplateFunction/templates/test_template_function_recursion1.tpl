{strip}
{function 'recursion1' level=0}
    {$level}
    {if $level < 5}
        {call name=recursion1 level=$level+1}
    {/if}
{/function}

{call name=recursion1}


{function 'recursion2' level=0}
    <br>2 level = {$level}
    {if $level < 5}
        {call name=recursion2 level=$level+1}
    {/if}
{/function}

{recursion2}
{/strip}