\$error\_reporting {#variable.error.reporting}
==================

When this value is set to a non-null-value it\'s value is used as php\'s
[`error_reporting`](https://www.php.net/error_reporting) level inside of
[`display()`](../../programmers/api-functions/api-display.md) and [`fetch()`](../../programmers/api-functions/api-fetch.md).

Smarty 3.1.2 introduced the
[`muteExpectedErrors()`](#api.mute.expected.errors) function. Calling
`\Smarty\Smarty::muteExpectedErrors();` after setting up custom error handling
will ensure that warnings and notices (deliberately) produced by Smarty
will not be passed to other custom error handlers. If your error logs
are filling up with warnings regarding `filemtime()` or `unlink()`
calls, please enable Smarty\'s error muting.

See also [debugging](#chapter.debugging.console) and
[troubleshooting](#troubleshooting).
