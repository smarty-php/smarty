{* Smarty *}

{assign_debug_info}

<SCRIPT language=javascript>
	_smarty_console = window.open("","console","width=560,height=600,resizable,scrollbars=yes");
	_smarty_console.document.write("<HTML><TITLE>Smarty Debug Console</TITLE><BODY bgcolor=#ffffff>");
	_smarty_console.document.write("<table border=0 width=100%>");
	_smarty_console.document.write("<tr bgcolor=#cccccc><th colspan=2>Smarty Debug Console</th></tr>");
	_smarty_console.document.write("<tr bgcolor=#cccccc><td colspan=2><b>included templates & config files:</b></td></tr>");
	{section name=templates loop=$_debug_tpls}
		_smarty_console.document.write("<tr bgcolor={if %templates.index% is even}#eeeeee{else}#fafafa{/if}><td colspan=2><tt>{section name=indent loop=$_debug_tpls[templates].depth}&nbsp;&nbsp;&nbsp;{/section}<font color={if $_debug_tpls[templates].type eq "template"}brown{else}green{/if}>{$_debug_tpls[templates].filename}</font></tt></td></tr>");
	{sectionelse}
		_smarty_console.document.write("<tr bgcolor=#eeeeee><td colspan=2><tt><i>no templates included</i></tt></td></tr>");	
	{/section}
	_smarty_console.document.write("<tr bgcolor=#cccccc><td colspan=2><b>assigned template variables:</b></td></tr>");
	{section name=vars loop=$_debug_keys}
		_smarty_console.document.write("<tr bgcolor={if %vars.index% is even}#eeeeee{else}#fafafa{/if}><td><tt><font color=blue>{ldelim}${$_debug_keys[vars]}{rdelim}</font></td><td>{if is_array($_debug_vals[vars])}<font color=green>Array</font> ({$_debug_vals[vars]|@count}){elseif empty($_debug_vals[vars])}<i>no value</i>{else}<font color=red>{$_debug_vals[vars]|truncate:50|regex_replace:"![\r\t\n]!":" "|escape|default:"<i>empty</i>"}</font>{/if}</tt></td></tr>");
	{sectionelse}
		_smarty_console.document.write("<tr bgcolor=#eeeeee><td colspan=2><tt><i>no template variables assigned</i></tt></td></tr>");	
	{/section}
	_smarty_console.document.write("<tr bgcolor=#cccccc><td colspan=2><b>assigned config file variables (outter template scope):</b></td></tr>");
	{section name=config_vars loop=$_debug_config_keys}
		_smarty_console.document.write("<tr bgcolor={if %config_vars.index% is even}#eeeeee{else}#fafafa{/if}><td><tt><font color=maroon>{ldelim}#{$_debug_config_keys[config_vars]}#{rdelim}</font></td><td>{if is_array($_debug_config_vals[config_vars])}<font color=green>Array</font> ({$_debug_config_vals[config_vars]|@count}){elseif empty($_debug_config_vals[config_vars])}<i>no value</i>{else}<font color=red>{$_debug_config_vals[config_vars]|truncate:50|regex_replace:"![\r\t\n]!":" "|escape|default:"<i>empty</i>"}</font>{/if}</tt></td></tr>");
	{sectionelse}
		_smarty_console.document.write("<tr bgcolor=#eeeeee><td colspan=2><tt><i>no config vars assigned</i></tt></td></tr>");	
	{/section}
	_smarty_console.document.write("</table>");
	_smarty_console.document.write("</BODY></HTML>");
	_smarty_console.document.close();
</SCRIPT>
