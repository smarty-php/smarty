{config_load file=test.conf}

Title: {#title#|capitalize}

{* A simple variable test *}
hello, my name is {$Name}

My interests are:
{section name=outer loop=$FirstName}
	{if %outer.index% is odd by 2}
		. {$outer/FirstName} {$outer/LastName}
	{else}
		* {$outer/FirstName} {$outer/LastName}
	{/if}
{sectionelse}
	none
{/section}
