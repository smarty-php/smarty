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
hello, my name is {$Name}

My interests are:
{section name=outer loop=$FirstName}
{if %outer.index% is odd by 2}
	{%outer.rownum%} . {$FirstName[outer]} {$LastName[outer]}
{else}
	{%outer.rownum%} * {$FirstName[outer]} {$LastName[outer]}
{/if}
{sectionelse}
	none
{/section}

testing section looped key values<br>
{section name=sec1 loop=$contacts}
	phone: {$contacts[sec1].phone}<br>
	fax: {$contacts[sec1].fax}<br>
	cell: {$contacts[sec1].cell}<br>
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

<form>
{html_select_date start_year=1998 end_year=2010}
</form>

This is an example of the html_select_time function:

<form>
{html_select_time use_24_hours=false}
</form>

This is an example of the html_options function:

<form>
<select name=states>
{html_options values=$option_values selected=$option_selected output=$option_output}
</select>
</form>

{myfunc attr1=5 attr2=$Name|upper}
{include file="footer.tpl"}
