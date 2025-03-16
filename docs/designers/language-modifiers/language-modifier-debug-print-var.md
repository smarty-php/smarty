# debug_print_var



Returns the value of the given variable in a human-readable format in HTML. 
Used in the [debug console](../chapter-debugging-console.md), but you can also use it in your template
while developing to see what is going on under the hood.

> **Note**
>
> Use for debugging only! Since you may accidentally reveal sensitive information or introduce vulnerabilities such as XSS using this
method never use it in production.

## Basic usage
```smarty
{$myVar|debug_print_var}
```


## Parameters

| Parameter | Type | Required | Description                                                            |
|-----------|------|----------|------------------------------------------------------------------------|
| 1         | int  | No       | maximum recursion depth if $var is an array or object (defaults to 10) |
| 2         | int  | No       | maximum string length if $var is a string (defaults to 40)             |

