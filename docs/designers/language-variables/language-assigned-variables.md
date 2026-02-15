# Variables assigned from PHP

Variables assigned from PHP are referenced by preceding them with a dollar
(`$`) sign.

## Examples

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty();

$smarty->assign('firstname', 'Doug');
$smarty->assign('lastname', 'Evans');
$smarty->assign('meetingPlace', 'New York');

$smarty->display('index.tpl');

```

`index.tpl` source:

```smarty
Hello {$firstname} {$lastname}, glad to see you can make it.
<br />
{* this will not work as $variables are case sensitive *}
This weeks meeting is in {$meetingplace}.
{* this will work *}
This weeks meeting is in {$meetingPlace}.
```
       
This above would output:

```html
Hello Doug Evans, glad to see you can make it.
<br />
This weeks meeting is in .
This weeks meeting is in New York.
```
      
## Associative arrays

You can also reference associative array variables by specifying the key
after a dot "." symbol.

```php
<?php
$smarty->assign('Contacts',
    array('fax' => '555-222-9876',
          'email' => 'zaphod@slartibartfast.example.com',
          'phone' => array('home' => '555-444-3333',
                           'cell' => '555-111-1234')
                           )
         );
$smarty->display('index.tpl');
```

`index.tpl` source:

```smarty
{$Contacts.fax}<br />
{$Contacts.email}<br />
{* you can print arrays of arrays as well *}
{$Contacts.phone.home}<br />
{$Contacts.phone.cell}<br />
```

this will output:

```html
555-222-9876<br />
zaphod@slartibartfast.example.com<br />
555-444-3333<br />
555-111-1234<br />
```

## Array indexes

You can reference arrays by their index, much like native PHP syntax.

```php
<?php
$smarty->assign('Contacts', array(
                           '555-222-9876',
                           'zaphod@slartibartfast.example.com',
                            array('555-444-3333',
                                  '555-111-1234')
                            ));
$smarty->display('index.tpl');
```

`index.tpl` source:

```smarty
{$Contacts[0]}<br />
{$Contacts[1]}<br />
{* you can print arrays of arrays as well *}
{$Contacts[2][0]}<br />
{$Contacts[2][1]}<br />
```

This will output:

```html
555-222-9876<br />
zaphod@slartibartfast.example.com<br />
555-444-3333<br />
555-111-1234<br />
```

## Objects

Properties of [objects](../../api/variables/objects.md) assigned from PHP
can be referenced by specifying the property name after the `->` symbol.

```smarty
name:  {$person->name}<br />
email: {$person->email}<br />
```

this will output:

```html
name:  Zaphod Beeblebrox<br />
email: zaphod@slartibartfast.example.com<br />
```

## Backed Enums (PHP 8.1+)

Smarty supports accessing properties of [backed enums](https://www.php.net/manual/en/language.enumerations.backed.php) introduced in PHP 8.1.

### Accessing Enum Properties

You can access the `name` and `value` properties of backed enum cases:

```smarty
{* Access enum case properties *}
<option id="{MyEnum::Foo->name}">{MyEnum::Foo->value}</option>
```

### Complete Example

```php
<?php
use Smarty\Smarty;

// Define a backed enum
enum Status: string {
    case Active = 'active';
    case Inactive = 'inactive';
    case Pending = 'pending';
}

$smarty = new Smarty();
$smarty->assign('currentStatus', Status::Active);
$smarty->display('template.tpl');
```

`template.tpl`:

```smarty
{* Display enum properties *}
Current status: {$currentStatus->name} (value: {$currentStatus->value})

{* Use in HTML attributes *}
<select name="status">
    <option value="{Status::Active->value}" {if $currentStatus->name === 'Active'}selected{/if}>Active</option>
    <option value="{Status::Inactive->value}" {if $currentStatus->name === 'Inactive'}selected{/if}>Inactive</option>
    <option value="{Status::Pending->value}" {if $currentStatus->name === 'Pending'}selected{/if}>Pending</option>
</select>
```

This would output:

```html
Current status: Active (value: active)

<select name="status">
    <option value="active" selected>Active</option>
    <option value="inactive">Inactive</option>
    <option value="pending">Pending</option>
</select>
```

### Integer-backed Enums

Integer-backed enums work the same way:

```php
<?php
enum Priority: int {
    case Low = 1;
    case Medium = 2; 
    case High = 3;
}

$smarty->assign('priority', Priority::High);
```

```smarty
{* Access integer enum properties *}
Priority level: {$priority->value} ({$priority->name})
```

Output:

```html
Priority level: 3 (High)
```

> **Note**: Backed enum support requires PHP 8.1 or higher. The enum must be registered or available in the current namespace.