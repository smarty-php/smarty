# Creating an extension

## Default extensions

In order to organize your custom tags and modifiers, you can create an Extension.
In fact, most of Smarty itself is organized into two extensions:

- the core extension, which provides the basic language tags such as `{if}`, `{for}` and `{assign}`. 
- the default extension, which provides all default modifiers such as `|escape`,  `|nl2br` and  `|number_format` 
  and tags such as `{html_image}`, `{mailto}` and `{textformat}` that are enabled by default, but not necessarily universal.

> ** Note **
> 
> There is also the 'BCPluginsAdapter' extension, which does not add any new functionality, but
> wraps calls to deprecated methods such as `Smarty\Smarty::addPluginsDir()` and `Smarty\Smarty::loadFilter()`.

## Writing your own extension

In order to write your own custom extension, you must write a class that implements `Smarty\Extension\ExtensionInterface`.
However, it is usually easier to extend `Smarty\Extension\Base` which provides empty implementation for each of the methods
required by `Smarty\Extension\ExtensionInterface`. This allows you to only override the method(s) you need.

Example:
```php
<?php

use Smarty\Extension\Base;

class MyExtension extends Base {

    public function getModifierCompiler(string $modifier): ?\Smarty\Compile\Modifier\ModifierCompilerInterface {

		switch ($modifier) {
			case 'array_escape': return new MyArrayEscapeModifierCompiler();
			case 'array_unescape': return new MyArrayUnescapeModifierCompiler();
        }

		return null;
	}
}

```
Another example, that would allow you to use any valid PHP callable as a modifier in your templates:

```php
<?php

use Smarty\Extension\Base;

class MyCallablePassThroughExtension extends Base {

    public function getModifierCallback(string $modifierName) {
    
        if (is_callable($modifierName)) {
            return $modifierName;
        }
    
		return null;
	}
}

```

Writing an extension allows you to add a group of tags, block tags and modifiers to the Smarty language.
It also allows you to register pre-, post- and output-filters in a structured way. 
The files in `src/Extension/` in the `smarty/smarty` dir should give you all the information you need to start
writing your own extension.

## Registering an extension

When you have written your extension, add it to a Smarty instance as follows:

```php
<?php

use Smarty\Smarty;

$smarty = new Smarty();

$smarty->addExtension(new MyCustomExtension());
```

This will add `MyCustomExtension` to the end of the extension list, meaning that you cannot override tags or modifiers
from one of Smarty's default extensions.

Should you wish to insert your extension at the top of the extension list, or create a very limited Smarty version that
only contains the core extension, you can use `Smarty\Smarty::setExtensions()` to override the list of extensions.

```php
<?php

use Smarty\Smarty;

$smarty = new Smarty();

$smarty->setExtensions([
    new Smarty\Extension\CoreExtension(),
    new MyCustomExtension(),
    new Smarty\Extension\DefaultExtension(),
]);
```