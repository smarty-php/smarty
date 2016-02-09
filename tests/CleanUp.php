<?php
/*
* This file is part of the Smarty PHPUnit tests.
*
*/
foreach (new RecursiveIteratorIterator(new RecursiveDirectoryIterator(dirname(__FILE__), FilesystemIterator::SKIP_DOTS),
                                       RecursiveIteratorIterator::CHILD_FIRST) as $path) {
    $p = $path->getPathname();
    if ((strpos($p, '\coverage') !== false) ||
        ((strpos($p, '\templates_c') === false) && (strpos($p, '\cache\\') === false) &&
            (strpos($p, '\templates_tmp') === false))
    ) {
        continue;
    }
    $p = $path->getPathname();
    $i = 1;
    //$path->isDir() && !$path->isLink() ? rmdir($path->getPathname()) : unlink($path->getPathname());
    if ($path->isDir() && !$path->isLink()) {
        if (strpos($p, '\templates_tmp') !== false) {
            rmdir($path->getPathname());
        } else {
            touch($path->getPathname() . '/dummy.txt');
        }
    } else {
        unlink($path->getPathname());
        if (strpos($p, '\templates_tmp') === false) {
            touch($path->getPath() . '/dummy.txt');
        }
    }
}

