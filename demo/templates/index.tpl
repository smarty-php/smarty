{include header.tpl}
{* A simple variable test *}
hello, my name is {$Name}.<br>
{if $Name eq "Joe"}
	I am Joe.<br>
{else}
	I am not Joe.<br>
{/if} 
<p>
Lets loop through some information:<br>
{section name="outside" $FirstName}
	current name is {$outside.LastName}, {$outside.FirstName}<br>
	{section name=inside $Class}
		current class is: {$inside.Class}<br>
	{/section}
	{if $outside.rownum.even}
		row is even<br>
	{else}
		row is odd<br>
	{/if}
	{if $outside.rownum.mod.2}
		row is evenly divisible by 2<br>
	{else}
		row is not evenly divisible by 2<br>
	{/if}
	{if $outside.rownum.even.2}
		row is even, grouped by 2<br>
	{else}
		row is odd, grouped by 2<br>
	{/if}
	<p>
{/section}

{section name=blah $loopvar2}
	test default: {htmlesc $blah.loopvar2}<br>
	a free standing section.<br>
{/section}

By default, php code is escaped:<br>
<?php print "hello"; ?>
<p>
{* the config function at work *}
<p>

{configload "config/index.conf"}
config var title: {configprint "title"}<br>
config var Name: {configprint "Name"}<br>
{configset "Name", $test}
test is {htmlesc $test}<br>
{configclear}
<p>
The following text is {htmlesc "<HTML><ESCAPED>"}.<br>
{* This is a template comment *}
<p>
{strip}
	All data within the<br>
		{ldelim}strip{rdelim} tags is stripped<br>
			of unneeded tabs, spaces and carriage returns<br>
				at the beginning and end of lines<br>
				in the html source (view this source)<br>
{/strip}

{include footer.tpl}
