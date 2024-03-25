# Streams

You can also use streams to call variables. *{$foo:bar}* will use the
*foo://bar* stream to get the template variable.

Using a PHP stream for a template variable resource from within a
template.

```smarty
{$foo:bar}
```

NB. Support for using streams to call variables is deprecated since Smarty v5.1 and will be removed
in a future version. 

See also [`Template Resources`](../resources.md)
