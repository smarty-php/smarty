# Static Classes

You can directly access static classes. The syntax is roughly the same as in
PHP.

> **Note**
>
> Direct access to PHP classes is not recommended. This ties the
> underlying application code structure directly to the presentation,
> and also complicates template syntax. It is recommended to register
> plugins which insulate templates from PHP classes/objects. Use at your
> own discretion.

## Examples

**class constant BAR**
```smarty
{assign var=foo value=myclass::BAR}
```

**method result**
```smarty
{assign var=foo value=myclass::method()} 
```

**method chaining**
```smarty
{assign var=foo value=myclass::method1()->method2}
```

**property bar of class myclass**
```smarty
{assign var=foo value=myclass::$bar} 
```

**using Smarty variable bar as class name**
```smarty
{assign var=foo value=$bar::method}
```
