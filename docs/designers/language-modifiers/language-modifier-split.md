# split

Splits a string into an array, using the optional second parameter as the separator.

## Basic usage

For `$chars` populated with `'abc'`, the following will produce a html list with 3 elements (a, b and c).
```smarty
<ol>
    {foreach $chars|split as $char}
        <li>{$char|escape}</li>
    {/foreach}
</ol>
```

## Parameters

| Parameter | Type   | Required | Description                                                                                                                  |
|-----------|--------|----------|------------------------------------------------------------------------------------------------------------------------------|
| 1         | string | No       | separator used to split the string on. Defaults to empty string, causing each character in the source string to be separate. |

## Examples


For `$ids` populated with `'1,2,3'`, the following will produce a html list with 3 elements (1, 2 and 3).
```smarty
<ol>
    {foreach $ids|split:',' as $id}
        <li>{$id|escape}</li>
    {/foreach}
</ol>
```