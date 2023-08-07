# Multiple caches per template

## Introduction

You can have multiple cache files for a single call to
`display()` or `fetch()`. 

Let's say that
a call to `$smarty->display('index.tpl')` may have several different output
contents depending on some condition, and you want separate caches for
each one. You can do this by passing a `$cache_id` as the second
parameter to the function call:

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

$my_cache_id = (int) $_GET['article_id'];

$smarty->display('index.tpl', $my_cache_id);
```


Above, we are passing the variable `$my_cache_id` to
[`display()`](#api.display) as the `$cache_id`. For each unique value of
`$my_cache_id`, a separate cache will be generated for `index.tpl`. In
this example, `article_id` was passed in the URL and is used as the
`$cache_id`.

> **Note**
>
> Be very cautious when passing values from a client (web browser) into
> Smarty or any PHP application. Although the above example of using the
> article_id from the URL looks handy, it could have bad consequences.
> The `$cache_id` is used to create a directory on the file system, so
> if the user decided to write a script that sends random article_id's at a rapid pace,
> this could possibly cause problems at the server level.
> Be sure to sanitize any data passed in before using it. In this example, you might want to check if
> the article_id is a valid ID in the database.

Be sure to pass the same `$cache_id` as the second parameter to
`isCached()` and `clearCache()`.

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

$my_cache_id = (int) $_GET['article_id'];

if (!$smarty->isCached('index.tpl', $my_cache_id)) {
    // ...
}

$smarty->display('index.tpl', $my_cache_id);
```

## Clearing specific caches

You can clear all caches for a particular `$cache_id` by passing NULL as
the first parameter to `clearCache()`.

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

// clear all caches with "sports" as the $cache_id
$smarty->clearCache(null, 'sports');

$smarty->display('index.tpl', 'sports');
```

In this manner, you can "group" your caches together by giving them the
same `$cache_id`.

## Advanced cache grouping

You can do more elaborate grouping by setting up `$cache_id` groups.
This is accomplished by separating each sub-group with a vertical bar
`|` in the `$cache_id` value. You can have as many sub-groups as you
like.

-   You can think of cache groups like a directory hierarchy. For
    instance, a cache group of `'a|b|c'` could be thought of as the
    directory structure `'/a/b/c/'`.

-   `clearCache(null, 'a|b|c')` would be like removing the files
    `'/a/b/c/*'`. `clearCache(null, 'a|b')` would be like removing the
    files `'/a/b/*'`.

-   If you specify a template name such as
    `clearCache('foo.tpl', 'a|b|c')` then Smarty will attempt to remove
    `'/a/b/c/foo.tpl'`.

-   You CANNOT remove a specified template name under multiple cache
    groups such as `'/a/b/*/foo.tpl'`, the cache grouping works
    left-to-right ONLY. You will need to group your templates under a
    single cache group hierarchy to be able to clear them as a group.

Cache grouping should not be confused with your template directory
hierarchy, the cache grouping has no knowledge of how your templates are
structured. So for example, if you have a template structure like
`themes/blue/index.tpl` and you want to be able to clear all the cache
files for the "blue" theme, you will need to create a cache group
structure that mimics your template file structure, such as
`display('themes/blue/index.tpl', 'themes|blue')`, then clear them with
`clearCache(null, 'themes|blue')`.

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

// clear all caches with 'sports|basketball' as the first two cache_id groups
$smarty->clearCache(null, 'sports|basketball');

// clear all caches with "sports" as the first cache_id group. This would
// include "sports|basketball", or "sports|(anything)|(anything)|(anything)|..."
$smarty->clearCache(null, 'sports');

// clear the foo.tpl cache file with "sports|basketball" as the cache_id
$smarty->clearCache('foo.tpl', 'sports|basketball');

$smarty->display('index.tpl', 'sports|basketball');
```

          
