# round

Rounds a number to the specified precision.

## Basic usage
```smarty
{3.14|round} # renders: 3
```

```smarty
{3.141592|round:2} # renders: 3.14
```

## Parameters

| Parameter | Type | Required | Description               |
|-----------|------|----------|---------------------------|
| 1         | int  | No       | precision (defaults to 0) |
| 2         | int  | No       | mode (defaults to 1)      |

If 'precision' is negative, the number is rounded to the nearest power of 10. See examples below.

The parameter 'mode' defines how the rounding is done. By default, 2.5 is rounded to 3, whereas 2.45 is rounded to 2.
You usually don't need to change this. For more details on rounding modes, 
see [PHP's documentation on round](https://www.php.net/manual/en/function.round).

## Examples

By passing `16` as the second parameter, you can force json_encode to always format the JSON-string as an object.
Without it, an array `$myArray = ["a","b"]` would be formatted as a javascript array:

```smarty
{$myArray|json_encode} # renders: ["a","b"]
{$myArray|json_encode:16} # renders: {"0":"a","1":"b"}
```