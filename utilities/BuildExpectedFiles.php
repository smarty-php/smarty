<?php
/**
 * Created by PhpStorm.
 * User: Uwe Tews
 * Date: 25.10.2015
 * Time: 23:58
 */
$sysplugins = array();
$iterator = new DirectoryIterator(dirname(__FILE__) . '/../libs/sysplugins');
foreach ($iterator as $file) {
    if (!$file->isDot() && 'php' == $file->getExtension()) {
        $filename = $file->getBasename();
        $sysplugins[ $filename ] = true;
    }
}
$plugins = array();
$iterator = new DirectoryIterator(dirname(__FILE__) . '/../libs/plugins');
foreach ($iterator as $file) {
    if (!$file->isDot() && 'php' == $file->getExtension()) {
        $filename = $file->getBasename();
        $plugins[ $filename ] = true;
    }
}
$code = file_get_contents(dirname(__FILE__) . '/../libs/sysplugins/smarty_internal_testinstall.php');
$expectedPlugins = '$expectedPlugins = ' . var_export($plugins, true);
$code = preg_replace('#\$expectedPlugins =[^;]+#', $expectedPlugins, $code);
$expectedSysplugins = '$expectedSysplugins = ' . var_export($sysplugins, true);
$code = preg_replace('#\$expectedSysplugins =[^;]+#', $expectedSysplugins, $code);
file_put_contents(dirname(__FILE__) . '/../libs/sysplugins/smarty_internal_testinstall.php', $code);
