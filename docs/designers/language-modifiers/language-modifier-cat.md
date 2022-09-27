# cat (modifier)

This value is concatenated to the given variable.

## Parameters

| Position | Type   | Required | Default | Description                                   |
|----------|--------|----------|---------|-----------------------------------------------|
| 1        | string | No       | *empty* | This value to catenate to the given variable. |

## Example

PHP-script:
```php
$smarty->assign('articleTitle', "Psychics predict world didn't end");
```
   
Where template is:
```smarty
   {$articleTitle|cat:' yesterday.'}
```

Will output:
```
    Psychics predict world didn't end yesterday.
```
