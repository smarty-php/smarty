<?php
/*
* This file is part of the Smarty PHPUnit tests.
*
*/
foreach (new RecursiveIteratorIterator(new RecursiveDirectoryIterator(dirname(__FILE__), FilesystemIterator::SKIP_DOTS)) as $path)
{
    $p = $path->getPathname();
    if ((strpos($p, '\templates_c') !== false) || (strpos($p, '\cache') !== false) ||
        (strpos($p, '\templates_tmp') !== false)) {
        $path2 = $path;
       $pp = $path->getPath();
        foreach (new RecursiveIteratorIterator(new RecursiveDirectoryIterator($path->getPath(). '\\', FilesystemIterator::SKIP_DOTS),
                                               RecursiveIteratorIterator::CHILD_FIRST) as $path2) {
            $p2 = $path2->getPathname();
            if ((strpos($p2, '\templates_c\\') !== false) || (strpos($p2, '\cache\\') !== false) ||
                (strpos($p2, '\templates_tmp\\') !== false)) {
                if ($path2->isDir() && !$path2->isLink()) {
                    rmdir($p2);
                } else {
                    unlink($p2);
                }
            }
        }
        $name = $pp. '\dummy.txt';

        file_put_contents($name,'dummy');
    }
}


