clearCache()

clears the cache for a specific template

Description
===========

void

clearCache

string

template

string

cache\_id

string

compile\_id

int

expire\_time

-   If you have [multiple caches](#caching.multiple.caches) for a
    template, you can clear a specific cache by supplying the `cache_id`
    as the second parameter.

-   You can also pass a [`$compile_id`](../api-variables/variable-compile-id.md) as a third
    parameter. You can [group templates together](#caching.groups) so
    they can be removed as a group, see the [caching section](../../api/caching/basics.md)
    for more information.

-   As an optional fourth parameter, you can supply a minimum age in
    seconds the cache file must be before it will get cleared.

    > **Note**
    >
    > Since Smarty version 3.1.14 it is possible to delete cache files
    > by their individual expiration time at creation by passing
    > constant SMARTY::CLEAR\_EXPIRED as fourth parameter.

<!-- -->


    <?php
    // clear the cache for a template
    $smarty->clearCache('index.tpl');

    // clear the cache for a particular cache id in an multiple-cache template
    $smarty->clearCache('index.tpl', 'MY_CACHE_ID');
    ?>

       

See also [`clearAllCache()`](../../programmers/api-functions/api-clear-all-cache.md) and
[`caching`](../../api/caching/basics.md) section.
