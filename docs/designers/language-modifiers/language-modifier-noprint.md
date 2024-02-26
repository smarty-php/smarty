# noprint

Always returns an empty string. This can be used to call a function or a method on an object that 
returns output, and suppress the output.

## Basic usage
```smarty
{$controller->sendEmail()|noprint}
```
