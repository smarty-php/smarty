# isset

Returns true if the variable(s) passed to it are different from null.

If multiple parameters are supplied then isset() will return true only if all of the parameters are 
not null.

## Basic usage
```smarty
{if $myVar|isset}all set!{/if}
```
