{* Smarty *}

{assign_debug_info}

<SCRIPT language=javascript>
	_smarty_console = window.open("","console","width=560,height=600,resizable,scrollbars=yes");
	_smarty_console.document.write("<HTML><TITLE>Smarty Debug Console</TITLE><BODY bgcolor=#ffffff>");
	_smarty_console.document.write("<table border=0 width=100%>");
	_smarty_console.document.write("<tr bgcolor=#cccccc><th colspan=2>Smarty Debug Console</th></tr>");
	_smarty_console.document.write("<tr bgcolor=#cccccc><td colspan=2><b>included templates:</b></td></tr>");
	{section name=templates loop=$_debug_tpls}
		_smarty_console.document.write("<tr bgcolor={if %templates.index% is even}#eeeeee{else}#fafafa{/if}><td colspan=2><tt><font color=blue>{$_debug_tpls[templates]}</font></tt></td></tr>");
	{/section}
	_smarty_console.document.write("<tr bgcolor=#cccccc><td colspan=2><b>assigned template variables:</b></td></tr>");
	{section name=vars loop=$_debug_keys}
		_smarty_console.document.write("<tr bgcolor={if %vars.index% is even}#eeeeee{else}#fafafa{/if}><td><tt><font color=blue>{$_debug_keys[vars]}</font></td><td>{if is_array($_debug_vals[vars])}<font color=green>Array</font> ({$_debug_vals[vars]|@count}){elseif empty($_debug_vals[vars])}<i>no value</i>{else}<font color=red>{$_debug_vals[vars]|truncate:50|regex_replace:"![\r\t\n]!":" "|escape|default:"<i>empty</i>"}</font>{/if}</tt></td></tr>");
	{/section}
	_smarty_console.document.write("</table>");
	_smarty_console.document.write("</BODY></HTML>");
	_smarty_console.document.close();
</SCRIPT>
