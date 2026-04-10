<?php
/*
* This file is part of the Smarty PHPUnit tests.
*
*/
/*
 * Smarty PHPUnit Bootstrap
 */
require_once __DIR__ . '/Config.php';
require_once __DIR__ . '/../vendor/autoload.php';

require_once 'PHPUnit_Smarty.php';
if (!ini_get('date.timezone')) {
    ini_set('date.timezone', 'Europe/Berlin');
}

// Clean the temp directory used for compiled templates, cache and templates_tmp
$smartyTestTempRoot = rtrim(sys_get_temp_dir(), DIRECTORY_SEPARATOR) . DIRECTORY_SEPARATOR . 'smarty-tests';
if (is_dir($smartyTestTempRoot)) {
    $di = new RecursiveDirectoryIterator($smartyTestTempRoot, RecursiveDirectoryIterator::SKIP_DOTS);
    $ri = new RecursiveIteratorIterator($di, RecursiveIteratorIterator::CHILD_FIRST);
    foreach ($ri as $file) {
        if ($file->isDir()) {
            @rmdir($file->getPathname());
        } else {
            @unlink($file->getPathname());
        }
    }
    @rmdir($smartyTestTempRoot);
}
mkdir($smartyTestTempRoot, 0775, true);

