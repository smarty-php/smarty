# join

Returns a string containing all the element of the given array 
with the separator string between each.

## Basic usage

For `$myArray` populated with `['a','b','c']`, the following will return the string `abc`.
```smarty
{$myArray|join}
```


## Parameters

| Parameter | Type   | Required | Description                                                 |
|-----------|--------|----------|-------------------------------------------------------------|
| 1         | string | No       | glue used between array elements. Defaults to empty string. |

## Examples


For `$myArray` populated with `[1,2,3]`, the following will return the string `1-2-3`.
```smarty
{$myArray|join:"-"}
```