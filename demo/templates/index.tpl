{include header.tpl}

{* A simple variable test *}
hello, my name is {$Name}.<br>
{if $Name eq "Joe"}
	I am {ldelim}Joe{rdelim}.<br>
{else}
	I am not Joe.<br>
{/if} 

{include footer.tpl}
