before {$foo} {$bar}<br>
{if $foo < 3}{include 'test_recursive_includes.tpl' foo=$foo+1}{/if}after {$foo} {$bar}<br>
