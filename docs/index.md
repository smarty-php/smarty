# Smarty Documentation
Smarty is a template engine for PHP, facilitating the separation of presentation (HTML/CSS) from application logic. 

It allows you to write **templates**, using **variables**, **modifiers**, **functions** and **comments**, like this:
```smarty
<h1>{$title|escape}</h1>

<p>
    The number of pixels is: {math equation="x * y" x=$height y=$width}.
</p>
```

When this template is rendered, with the value "Hello world" for the variable $title, 640 for $width, 
and 480 for $height, the result is:
```html
<h1>Hello world</h1>

<p>
    The number of pixels is: 307200.
</p>
```

## Getting Started
- [Getting Started](./getting-started.md)
- [Philosophy](./philosophy.md) - or "Why do I need a template engine?"
- [Features](./features.md) - or "Why do I want Smarty?"

## Help
- [Search or create an issue](https://github.com/smarty-php/smarty/issues)
- [Upgrading from an older version](upgrading.md)
- [Some random tips & tricks](./appendixes/tips.md)
- [Troubleshooting](./appendixes/troubleshooting.md)

## Source code
- [Smarty repository at GitHub](https://github.com/smarty-php/smarty)