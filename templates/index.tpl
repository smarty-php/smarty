{* A simple variable test *}
hello, my name is {$Name}.<br>
My interests are:
{section name=outer loop=$FirstName}
	{if %outer.index% is even by 2}
		. {$outer/FirstName} {$outer/LastName}
	{else}
		* {$outer/FirstName} {$outer/LastName}
	{/if}
{sectionelse}
	none
{/section}
