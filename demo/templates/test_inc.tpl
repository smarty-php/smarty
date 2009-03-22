Test of { include } and local/global variable scope
<br>the original value of $foo = {$foo}
<br>
{assign var=foo2 value='yzx'}
<br> foo2 before { include } = {$foo2}
{include file='test_inc2.tpl'}
<br>
<br>Here we are back in test_inc.tpl
<br>$foo has its old value = {$foo}
<br>this is $foo2 a global variable created in test_inc2.tpl = {$foo2}
<br>{if isset($foo3)} $foo3 must be unknow here {/if}
<br>
<br>Test include with parent scope
{include file='test_inc2.tpl' scope='parent'}
<br>Here we are back in test_inc.tpl
<br>$foo has its new value = {$foo}
<br>this is $foo2 a global variable created in test_inc2.tpl = {$foo2}
<br>this is $foo3 a normal variable created in test_inc2.tpl = {$foo3}
<br>
<br>Test include with root scope
{include file='test_inc2.tpl' scope='root'}
<br>Here we are back in test_inc.tpl
<br>$foo has its new value = {$foo}
<br>this is $foo2 a global variable created in test_inc2.tpl = {$foo2}
<br>this is $foo3 a normal variable created in test_inc2.tpl = {$foo3}
