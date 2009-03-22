<pre>SMARTY SMOKE TEST

VARIABLE TESTS:

$foo is {$foo}

$baz[1] is {$baz[1]}

$blah['b'] is {$blah['b']}

$blah[$baz[1]] is {$blah[$baz[1]]}

$foo.$baz[1] is {$foo.$baz[1]}

$foo.$foo is {$foo.$foo}

{"foo"}

OBJECT TESTS:

$myobj->foo is {$myobj->foo}

$myobj->test is {$myobj->test}
$myobj->test() is {$myobj->test()}
$myobj->test(1) is {$myobj->test(1)}
$myobj->test(1,'two') is {$myobj->test(1,'two')}
$myobj->test(count($baz)) is {$myobj->test(count($baz))}
$myobj->test($myobj->test(count($baz))) is {$myobj->test($myobj->test(count($baz)))}
$myobj->test($foo|escape) is {$myobj->test($foo|escape)}

PHP TESTS:


COMMENT TESTS:

{* this is a comment *}
{* another $foo comment *}
{* another <?=$foo?> comment *}
{* multi line
   comment *}
{* /* foo */ *}
A{* comment *}B
C
D{* comment *}
{* comment *}E
F
G{* multi
 line *}H
I{* multi
line *}
J

ASSIGN:

A
{assign var=zoo value="blah"}
B
C{assign var=zoo value="blah"}D
E{assign var=zoo value="blah"}
F
G
{assign var=zoo value="blah"}H
{assign var=zoo value="joe{$myobj->test(1)}bar"}

zoo is {$zoo}

SPACING TESTS:

{$foo}

{$foo}{$foo}

A{$foo}

A{$foo}B

{$foo}B

IF TESTS:

{if $foo eq "baz"}
  IS BAZ
{elseif $foo == "lala"}
  IS LALA
{else}
  IS NONE
{/if}

{if $myint+5 EQ 9}
  IS NINE
{else}
  IS NOT NINE
{/if}

{if $myint + 5 eq 9}
  IS NINE
{else}
  IS NOT NINE
{/if}

{if count($baz)-2 eq 1}
  IS ONE
{else}
  IS NOT ONE
{/if}

{if $foo.$foo2 eq "barbar2"}
  IS BARBAR2
{else}
  IS NOT BARBAR2
{/if}

{if $not_logged}
  NOT LOGGED
{/if}

{if "joe{$myobj->test(1)}bar"}
  FOO
{/if}

TEST INCLUDE:

{include file="header.tpl" gee="joe"}
{include file="header.tpl" gee="joe $foo bar"}
{include file="header.tpl" gee="joe{$foo}bar"}
{include file="header.tpl" gee="joe{$foo.$foo}bar"}
{include file="header.tpl" gee="joe{$myobj->test(1)}bar"}
{include file=$includeme}
{include file=$includeme gee="joe"}
{include file="{$top}.tpl"}

JAVSCRIPT TEST

<script language="javascript">
  function sayhi()
  {
    alert('hi there!');
  }
  function foobar() { alert('foobar'); }
</script>

MATH TESTS:

$one+2 is {$one+2}
$one + 2 - $one is {$one + 2 - $one}
$one*$one is {$one*$one}
$one/$one is {$one/$one}
abs(-$one) is {abs(-$one)}

$footest is {$footest}

FOREACH TESTS:

{foreach from=$blah key="idx" item="val"}
  $idx/$val is {$idx}/{$val}
{/foreach}

TEST FINISHED
</pre>
