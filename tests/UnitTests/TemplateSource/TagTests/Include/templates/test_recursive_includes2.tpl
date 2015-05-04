before {$foo} {$bar}<br>
{if $foo < 4}{include 'test_recursive_includes_pass.tpl' foo=$foo+1}{/if}after {$foo} {$bar}<br>
