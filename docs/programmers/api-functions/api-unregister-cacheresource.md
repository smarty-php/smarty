unregisterCacheResource()

dynamically unregister a CacheResource plugin

Description
===========

void

unregisterCacheResource

string

name

Pass in the `name` of the CacheResource.


    <?php

    $smarty->unregisterCacheResource('mysql');

    ?>

       

See also [`registerCacheResource()`](../../programmers/api-functions/api-register-cacheresource.md) and
the [Custom CacheResource Implementation](#caching.custom) section.
