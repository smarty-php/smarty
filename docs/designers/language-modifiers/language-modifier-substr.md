# substr

Returns a part (substring) of the given string starting at a given offset.

## Basic usage
```smarty
{"Smarty"|substr:2} # renders: arty
{"Smarty"|substr:2:3} # renders: art
```

## Parameters

| Parameter | Type | Required | Description                                         |
|-----------|------|----------|-----------------------------------------------------|
| 1         | int  | yes      | offset (zero based, can be negative)                |
| 2         | int  | no       | length of substring returned (unlimited of omitted) |


## Examples

When used with a negative offset, the substring starts n characters from the end of the string counting backwards.
```smarty
{"Smarty"|substr:-2} # renders: ty
{"Smarty"|substr:-2:1} # renders: t
```