isCached()

returns true if there is a valid cache for this template

Description
===========

bool

isCached

string

template

string

cache\_id

string

compile\_id

-   This only works if [`$caching`](#variable.caching) is set to one of
    `\Smarty\Smarty::CACHING_LIFETIME_CURRENT` or
    `\Smarty\Smarty::CACHING_LIFETIME_SAVED` to enable caching. See the
    [caching section](../../api/caching/basics.md) for more info.

-   You can also pass a `$cache_id` as an optional second parameter in
    case you want [multiple caches](#caching.multiple.caches) for the
    given template.

-   You can supply a [`$compile id`](../api-variables/variable-compile-id.md) as an
    optional third parameter. If you omit that parameter the persistent
    [`$compile_id`](../api-variables/variable-compile-id.md) is used if its set.

-   If you do not want to pass a `$cache_id` but want to pass a
    [`$compile_id`](../api-variables/variable-compile-id.md) you have to pass NULL as a
    `$cache_id`.

> **Note**
>
> If `isCached()` returns TRUE it actually loads the cached output and
> stores it internally. Any subsequent call to
> [`display()`](../../programmers/api-functions/api-display.md) or [`fetch()`](../../programmers/api-functions/api-fetch.md) will return
> this internally stored output and does not try to reload the cache
> file. This prevents a race condition that may occur when a second
> process clears the cache between the calls to `isCached()` and to
> [`display()`](../../programmers/api-functions/api-display.md) in the example above. This also means
> calls to [`clearCache()`](../../programmers/api-functions/api-clear-cache.md) and other changes of the
> cache-settings may have no effect after `isCached()` returned TRUE.


    <?php
    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    if(!$smarty->isCached('index.tpl')) {
    // do database calls, assign vars here
    }

    $smarty->display('index.tpl');
    ?>

       


    <?php
    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    if(!$smarty->isCached('index.tpl', 'FrontPage')) {
      // do database calls, assign vars here
    }

    $smarty->display('index.tpl', 'FrontPage');
    ?>

       

See also [`clearCache()`](../../programmers/api-functions/api-clear-cache.md),
[`clearAllCache()`](../../programmers/api-functions/api-clear-all-cache.md)
and [caching section](../../api/caching/basics.md).
