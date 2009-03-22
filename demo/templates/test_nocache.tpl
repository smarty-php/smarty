Test caching and nocache attribute
<br>cached time is {time()}
<br>nocached time by '{time() nocache=true}' {time() nocache=true}
<br>
<br>calling '{include file="nocache_inc.tpl" caching_lifetime=25}' {include file="nocache_inc.tpl" caching_lifetime=25}
<br>
<br>calling '{include file="nocache_inc.tpl" nocache=true}' {include file="nocache_inc.tpl" nocache=true}
{nocache}
{assign var=a value=1}
<br>{if $a < 5}{$a|truncate} lt 5{else}a ge 5{/if}
{/nocache}