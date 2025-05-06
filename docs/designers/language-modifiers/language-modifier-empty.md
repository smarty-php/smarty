# empty

Returns true if var does not exist or has a value that is empty or equal to zero, aka falsey, see conversion to boolean. Otherwise returns false.

## Basic usage

```smarty
{if $myVar|empty}it's an empty variable{/if}
{if empty($myVar)}it's an empty variable{/if}
```
