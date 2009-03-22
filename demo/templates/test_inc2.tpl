<br>Here starts test_inc2.tpl
<br>$foo in test_inc2 before changing = {$foo}
{assign var=foo value="bah"}
<br>$foo in test_inc2 after changing = {$foo}
{assign var=foo2 value="bla" global=true}
{assign var=foo3 value="foo3 was set"}
