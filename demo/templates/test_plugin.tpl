Test of function plugin
{nocache}
{counter assign=foo start=10 skip=5}
<br>{$foo}
{counter}
<br>{$foo}
{/nocache}
