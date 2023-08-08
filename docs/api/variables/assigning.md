# Assigning variables

Templates start to become really useful once you know how to use variables.

## Basic assigning
Let's revisit the example from the [basics section](../basics.md). The following script assigns a value to 
the 'companyName' variable and renders the template:

```php
<?php

use Smarty\Smarty;
$smarty = new Smarty();

$smarty->assign('companyName', 'AC & ME Corp.');

$smarty->display('footer.tpl');
```

footer.tpl:
```smarty
<small>Copyright {$companyName|escape}</small>
```

Smarty will apply the [escape modifier](../../designers/language-modifiers/language-modifier-escape.md)
to the value assigned to the variable
`companyName` and replace `{$companyName|escape}` with the result.

```html
<small>Copyright AC &amp; ME Corp.</small>
```

Using `$smarty->assign()` is the most common way of assigning data to templates, but there are several other methods.

## Appending data to an existing variable
Using `append()`, you can add data to an existing variable, usually an array.

If you append to a string value, it is converted to an array value and
then appended to. You can explicitly pass name/value pairs, or
associative arrays containing the name/value pairs. If you pass the
optional third parameter of TRUE, the value will be merged with the
current array instead of appended.

Examples:

```php
<?php
// This is effectively the same as assign()
$smarty->append('foo', 'Fred');
// After this line, foo will now be seen as an array in the template
$smarty->append('foo', 'Albert');

$array = [1 => 'one', 2 => 'two'];
$smarty->append('X', $array);
$array2 = [3 => 'three', 4 => 'four'];
// The following line will add a second element to the X array
$smarty->append('X', $array2);

// passing an associative array
$smarty->append(['city' => 'Lincoln', 'state' => 'Nebraska']);
```

## Assigning to template objects
When you use a template objects, as explained in [rendering a template](../rendering.md#creating-a-template-object),
you can assign data to the template objects directly instead of assigning it to Smarty. This way, you can use different
sets of data for different templates.

For example:
```php
<?php

use Smarty\Smarty;
$smarty = new Smarty();

$tplBlue = $smarty->createTemplate('blue.tpl');
$tplBlue->assign('name', 'The one');
$tplBlue->display();

$tplRed = $smarty->createTemplate('red.tpl');
$tplRed->assign('name', 'Neo');
$tplRed->display();
```

## Using data objects
For more complex use cases, Smarty supports the concept of data objects.
Data objects are containers to hold data. Data objects can be attached to templates when creating them.
This allows for fine-grained re-use of data.

For example:
```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

// create a data object
$data = $smarty->createData();

// assign variable to the data object
$data->assign('name', 'Neo');

// create template object which will use variables from the data object
$tpl = $smarty->createTemplate('index.tpl', $data);

// display the template
$tpl->display();
```

## Clearing assigned data
When re-using templates, you may need to clear data assigned in a previous run. Use `clearAllAssign()` to 
clear the values of all assigned variables on data objects, template objects or the Smarty object.

Examples:
```php
<?php
// assigning data to the Smarty object
$smarty->assign('Name', 'Fred');
// ...
$smarty->clearAllAssign();

// using a data object
$data = $smarty->createData();
$data->assign('name', 'Neo');
// ...
$data->clearAllAssign();

// using a template
$tplBlue = $smarty->createTemplate('blue.tpl');
$tplBlue->assign('name', 'The one');
// ...
$tplBlue->clearAllAssign();
```

Note that there it's only useful to clear assigned data if you:

1. repeatedly re-use templates, and
2. the variables used may change on each repetition

If your script simply runs once and then ends, or you always assign the same variables, clearing assigned data 
is of no use.