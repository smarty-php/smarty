# raw

Prevents variable escaping when [auto-escaping](../../api/configuring.md#enabling-auto-escaping) is activated.

## Basic usage
```smarty
{$myVar|raw}
```

Alternatively, the `nofilter` tag flag disables auto-escaping, as well as any
variable filter, for the whole tag:

```smarty
{$myVar nofilter}
```
