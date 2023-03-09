# Streams

You can also use streams to call variables. *{$foo:bar}* will use the
*foo://bar* stream to get the template variable.

Using a PHP stream for a template variable resource from within a
template.

```smarty
{$foo:bar}
```

See also [`Template Resources`](../resources.md)
