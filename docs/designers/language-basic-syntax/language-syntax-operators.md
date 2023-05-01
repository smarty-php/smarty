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

## Ternary
You can use the `?:` (or ternary) operator to test one expression and present the value
of the second or third expression, based on the result of te test.

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