{config_load file=test.conf section="setup"}
{include file=header.tpl title=foo}

<PRE>

{* bold and title are read from the config file *}
{if #bold#}<b>{/if}
{* capitalize the first letters of each word of the title *}
Title: {#title#|capitalize}
{if #bold#}</b>{/if}

the value of $SCRIPT_NAME is {$SCRIPT_NAME} 

{* A simple variable test. print $Name in uppercase *}
hello, my name is {$Name|upper}

My interests are:
{section name=outer loop=$FirstName}
{if %outer.index% is odd by 2}
	{%outer.rownum%} . {$outer/FirstName} {$outer/LastName}
{else}
	{%outer.rownum%} * {$outer/FirstName} {$outer/LastName}
{/if}
{sectionelse}
	none
{/section}

testing section looped key values<br>
{section name=sec1 loop=$contacts}
	phone: {$sec1/contacts.phone}<br>
	fax: {$sec1/contacts.fax}<br>
	cell: {$sec1/contacts.cell}<br>
{/section}
<p>

testing strip tags
{strip}
<table border=0>
	<tr>
		<td>
			<A HREF="{$SCRIPT_NAME}">
			<font color="red">This is a  test     </font>
			</A>
		</td>
	</tr>
</table>
{/strip}

</PRE>

This is an example of the html_select_date function:

</PRE>
<form>
{html_select_date start_year=1998 end_year=2010}
</form>
<PRE>

This is an example of the html_options function:
</PRE>
<form>
<select name=states>
{html_options values=$option_values selected=$option_selected output=$option_output}
</select>
</form>
