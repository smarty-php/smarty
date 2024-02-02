# Basics

## Installation
For installation instructies, please see the [getting started section](../getting-started.md).

## Rendering a template
Here's how you create an instance of Smarty in your PHP scripts:
```php
<?php

require 'vendor/autoload.php';
use Smarty\Smarty;
$smarty = new Smarty();
```

You now have a Smarty object that you can use to render templates.

```php
<?php

require 'vendor/autoload.php';
use Smarty\Smarty;
$smarty = new Smarty();

$smarty->display('string:The current smarty version is: {$smarty.version}.');
// or 
echo $smarty->fetch('string:The current smarty version is: {$smarty.version}.');
```

## Using file-based templates
You probably want to manage your templates as files. Create a subdirectory called 'templates' and
then configure Smarty to use that:

```php
<?php

require 'vendor/autoload.php';
use Smarty\Smarty;
$smarty = new Smarty();

$smarty->setTemplateDir(__DIR__ . '/templates');
```

Say you have a template file called 'version.tpl', stored in the 'templates' directory like this:
```smarty
<h1>Hi</h1>
The current smarty version is: {$smarty.version|escape}.
```

You can now render this, using:
```php
<?php

require 'vendor/autoload.php';
use Smarty\Smarty;
$smarty = new Smarty();

$smarty->setTemplateDir(__DIR__ . '/templates');
$smarty->display('version.tpl');
```

## Assigning variables

Templates start to become really useful once you add variables to the mix.

Create a template called 'footer.tpl' in the 'templates' directory like this:
```smarty
<small>Copyright {$companyName|escape}</small>
```

Now assign a value to the 'companyName' variable and render your template like this:

```php
<?php

require 'vendor/autoload.php';
use Smarty\Smarty;
$smarty = new Smarty();

$smarty->setTemplateDir(__DIR__ . '/templates');
$smarty->assign('companyName', 'AC & ME Corp.');
$smarty->display('footer.tpl');
```

Run this, and you will see:

```html
<small>Copyright AC &amp; ME Corp.</small>
```

Note how the [escape modifier](../designers/language-modifiers/language-modifier-escape.md) 
translated the `&` character into the proper HTML syntax `&amp;`.
Read more about auto-escaping in the [next section](./configuring.md).