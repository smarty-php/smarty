# {while}

`{while}` loops in Smarty have much the same flexibility as PHP
[while](https://www.php.net/while) statements, with a few added features for
the template engine. Every `{while}` must be paired with a matching
`{/while}`. All [operators](../language-basic-syntax/language-syntax-operators.md) are recognized, such as *==*,
*\|\|*, *or*, *&&*, *and*, etc and you can use modifiers as functions, such as *is_array()*.

## Examples
```smarty
{while $foo > 0}
  {$foo--}
{/while}
```

The above example will count down the value of $foo until 1 is reached.

See also [`{foreach}`](./language-function-foreach.md),
[`{for}`](./language-function-for.md) and
[`{section}`](./language-function-section.md).
