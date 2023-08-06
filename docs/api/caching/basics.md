# Caching

Caching is used to speed up the rendering of a template by saving and re-using the output. 

If a cached version of the call is available, that is displayed instead of
regenerating the output. Caching can speed things up tremendously,
especially templates with longer computation times. 

Since templates can include or extend other templates, one
cache file could conceivably be made up of several template files,
config files, etc.

> ** Note **
> 
> Since templates are dynamic, it is important to be careful what you are
> caching and for how long. For instance, if you are displaying the front
> page of your website that does not change its content very often, it
> might work well to cache this page for an hour or more. On the other
> hand, if you are displaying a page with a timetable containing new
> information by the minute, it would not make sense to cache this page.

## Setting Up Caching

The first thing to do is enable caching by calling `Smarty::setCaching()` with either
`\Smarty\Smarty::CACHING_LIFETIME_CURRENT` or `\Smarty\Smarty::CACHING_LIFETIME_SAVED`.
Or with `\Smarty\Smarty::CACHING_OFF` to disable caching again.

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

// enable caching, using the current lifetime (see below)
$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

// enable caching, using the lifetime set when the cache was saved (see below)
$smarty->setCaching(Smarty::CACHING_LIFETIME_SAVED);

// disable caching
$smarty->setCaching(Smarty::CACHING_OFF);

$smarty->display('index.tpl');
```

With caching enabled, the function call to `$smarty->display('index.tpl')` will
render the template as usual, but also saves a copy of its output. On the
next call to `$smarty->display('index.tpl')`, the cached copy will be used
instead of rendering the template again.

> **Note**
>
> By default, Smarty saved its caches as files in a dir called `cache` relative to the current 
> directory. The default directory can be changed using `$smarty->setCacheDir('/some/cache/dir');` 
> The files are named similar
> to the template name. Although they end in the `.php` extension, they
> are not intended to be directly executable. Do not edit these files!

## Cache lifetime

Each cached page has a limited lifetime. The default value is 3600
seconds, or one hour. After that time expires, the cache is regenerated.

You can change the lifetime as follows:
```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT); 
// or $smarty->setCaching(Smarty::CACHING_LIFETIME_SAVED);

// set the cache_lifetime to 5 minutes
$smarty->setCacheLifetime(5 * 60);
```

Setting caching to a value of `\Smarty\Smarty::CACHING_LIFETIME_CURRENT` tells Smarty to use
the current lifetime to determine if the cache has expired.

A value of `\Smarty\Smarty::CACHING\_LIFETIME\_SAVED` tells Smarty to use the lifetime value at the time the
cache was generated. This way you can set the just before rendering a template to have granular control over
when that particular cache expires.

An example:
```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

// retain current cache lifetime for each specific display call
$smarty->setCaching(Smarty::CACHING_LIFETIME_SAVED);

// set the cache_lifetime for index.tpl to 5 minutes
$smarty->setCacheLifetime(300);
$smarty->display('index.tpl');

// set the cache_lifetime for home.tpl to 1 hour
$smarty->setCacheLifetime(3600);
$smarty->display('home.tpl');

// NOTE: the following $cache_lifetime setting will not work when $caching
// is set to Smarty::CACHING_LIFETIME_SAVED.
// The cache lifetime for home.tpl has already been set
// to 1 hour, and will no longer respect the value of $cache_lifetime.
// The home.tpl cache will still expire after 1 hour.
$smarty->setCacheLifetime(30); // 30 seconds
$smarty->display('home.tpl');
```

## Compile check

By default, every template file and config file that is involved with the cache file
is checked for modification. If any of the files have been modified
since the cache was generated, the cache is immediately regenerated.

This is a computational overhead, so for optimum performance, disable this on a production environment:

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);
$smarty->setCompileCheck(Smarty::COMPILECHECK_OFF);

$smarty->display('index.tpl');
```

## Checking if a template is cached

Smarty's `isCached() method can be used to test if a
template has a valid cache or not. If you have a cached template that
requires something like a database fetch, you can use this to skip that
process.

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

if (!$smarty->isCached('index.tpl')) {
    // No cache available, do variable assignments here.
    $smarty->assign('data', do_expensive_database_calls());
}

$smarty->display('index.tpl');
```

## Nocache-blocks
You can keep parts of a page dynamic (disable caching) with the
[`{nocache}{/nocache}`](../../designers/language-builtin-functions/language-function-nocache.md) block function, 
or by using the `nocache` parameter for most template functions.

Let's say the whole page can be cached except for a banner that is
displayed down the side of the page. By using a [`{nocache}{/nocache}`](../../designers/language-builtin-functions/language-function-nocache.md)
block for the banner, you can
keep this element dynamic within the cached content.

## Clearing the cache
You can clear all the cache files with Smarty's `clearAllCache()` method, or individual cache
files with the `clearCache()` method.

```php
<?php
use Smarty\Smarty;
$smarty = new Smarty;

$smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

// clear only cache for index.tpl
$smarty->clearCache('index.tpl');

// clear out all cache files
$smarty->clearAllCache();

// clear out all cache files older than one hour
$smarty->clearAllCache(3600);

// or, clear all expired caches
$smarty->clearAllCache(Smarty::CLEAR_EXPIRED);
```

        
