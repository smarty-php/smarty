# number_format

Allows you to format a number using decimals and a thousands-separator. By default, the number of decimals is 0 
and the number is rounded.

## Basic usage
```smarty
{$num  = 2000.151}
{$num|number_format} # renders: 2,000
```


## Parameters

| Parameter | Type   | Required | Description                           |
|-----------|--------|----------|---------------------------------------|
| 1         | int    | No       | number of decimals (defaults to 0)    |
| 2         | string | No       | decimal separator (defaults to ".")   |
| 3         | string | No       | thousands-separator (defaults to ",") |


## Examples

```smarty
{$num  = 2000.151}
{$num|number_format:2} # renders: 2,000.15
```

```smarty
{$num  = 2000.151}
{$num|number_format:2:".":""} # renders: 2000.15
```