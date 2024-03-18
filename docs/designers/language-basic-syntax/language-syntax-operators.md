# Operators

## Basic

Various basic operators can be applied directly to variable values.

## Examples
```smarty
{$foo + 1}

{$foo * $bar}

{$foo->bar - $bar[1] * $baz->foo->bar() -3 * 7}

{if ($foo + $bar.test % $baz * 134232 + 10 + $b + 10)}
    ...
{/if}

{$foo = $foo + $bar}
```

> **Note**
>
> Although Smarty can handle some very complex expressions and syntax,
> it is a good rule of thumb to keep the template syntax minimal and
> focused on presentation. If you find your template syntax getting too
> complex, it may be a good idea to move the bits that do not deal
> explicitly with presentation to PHP by way of plugins or modifiers.

## List
The following is a list of recognized operators, which must be
separated from surrounding elements by spaces. Note that items listed in
\[brackets\] are optional. PHP equivalents are shown where applicable.

| Operator           | Alternates | Syntax Example       | Meaning                        | PHP Equivalent     |
|--------------------|------------|----------------------|--------------------------------|--------------------|
| ==                 | eq         | $a eq $b             | equals                         | ==                 |
| !=                 | ne, neq    | $a neq $b            | not equals                     | !=                 |
| >                  | gt         | $a gt $b             | greater than                   | >                  |
| <                  | lt         | $a lt $b             | less than                      | <                  |
| >=                 | gte, ge    | $a ge $b             | greater than or equal          | >=                 |
| <=                 | lte, le    | $a le $b             | less than or equal             | <=                 |
| ===                |            | $a === 0             | check for identity             | ===                |
| !                  | not        | not $a               | negation (unary)               | !                  |
| %                  | mod        | $a mod $b            | modulo                         | %                  |
| is \[not\] div by  |            | $a is not div by 4   | divisible by                   | $a % $b == 0       |
| is \[not\] even    |            | $a is not even       | \[not\] an even number (unary) | $a % 2 == 0        |
| is \[not\] even by |            | $a is not even by $b | grouping level \[not\] even    | ($a / $b) % 2 == 0 |
| is \[not\] odd     |            | $a is not odd        | \[not\] an odd number (unary)  | $a % 2 != 0        |
| is \[not\] odd by  |            | $a is not odd by $b  | \[not\] an odd grouping        | ($a / $b) % 2 != 0 |
| is in              |            | $a is in $b          | exists in array                | in_array($a, $b)   |
| is \[not\] in      |            | $a is not in $b      | does not exist in array        | !in_array($a, $b)  |

## Ternary
You can use the `?:` (or ternary) operator to test one expression and present the value
of the second or third expression, based on the result of the test.

In other words:
```smarty
{$test ? "OK" : "FAIL"}
```
will result in OK if `$test` is set to true, and in FAIL otherwise.

There is also a shorthand `?:` operator:
```smarty
{$myVar ?: "empty"}
```
will result in 'empty' if `$myVar` is not set or set to something that evaluates to false, such as an empty string.
If `$myVar` is set to something that evaluates to true, the value of `$myVar` is returned. So, the following will 
return 'hello':
```smarty
{$myVar="hello"}
{$myVar ?: "empty"}
```

## Testing for null
If "something that evaluates to false" is to broad a test for you, you can use the `??` (or null coalescing) operator 
to trigger only if the tested value is undefined or set to null.
```smarty
{$myVar ?? "empty"}
```
will result in 'empty' if `$myVar` is not set or set to null.
If `$myVar` is set to something that evaluates to anything else, the value of `$myVar` is returned. So, the following will
return an empty string (''):
```smarty
{$myVar=""}
{$myVar ?: "this is not shown"}
```