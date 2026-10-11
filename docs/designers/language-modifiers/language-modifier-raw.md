# raw

Prevents variable escaping when [auto-escaping](../../api/configuring.md#enabling-auto-escaping) is activated.

## Basic usage
```smarty
{$myVar|raw}
```

## Scope
`|raw` only prevents escaping if it applies to the printed value as a whole.
If it is used on just a part of an expression, the whole value is escaped:

```smarty
{* both $foo and $bar are escaped *}
{$foo|cat:($bar|raw)}

{* neither $foo nor $bar is escaped *}
{$foo|cat:$bar|raw}
```
