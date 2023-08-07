# Custom cache storage layers

As an alternative to using the default file-based caching mechanism, you
can specify a custom cache implementation that will be used to read,
write and clear cached files.

With a custom cache implementation you could replace the slow filesystem by a
faster storage engine, centralize the cache to be accessible to multiple
servers.

Smarty requires implementations to extend `\Smarty\Cacheresource\Base`, but encourages you to either extend 
`\Smarty\Cacheresource\Custom` or `\Smarty\Cacheresource\KeyValueStore`.

- `\Smarty\Cacheresource\Custom` is a simple API directing all read, write,
clear calls to your implementation. This API allows you to store
wherever and however you deem fit. 
- `\Smarty\Cacheresource\KeyValueStore` allows you to turn any 
KeyValue-Store (like APC or Memcache) into a full-featured
CacheResource implementation. Everything around deep
cache-groups like "a|b|c" is being handled for you in a way that
guarantees clearing the cache-group "a" will clear all nested groups 
as well - even though KeyValue-Stores don't allow this kind of
hierarchy by nature.

Custom CacheResources must be registered on
runtime with `Smarty\Smarty::setCacheResource()`:

```php
<?php

use Smarty\Smarty;
$smarty = new Smarty();

$smarty->setCacheResource(new My_CacheResource_Mysql());
```
       
