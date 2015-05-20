<?php
/*
* This file is part of the Smarty PHPUnit tests.
*
*/
/*
 * Smarty PHPUnit Bootstrap
 */
// Locate Autoloader or SmartyBC class and load it
if (is_file(__DIR__ . '/../smarty/libs/Autoloader.php')) {
    require_once __DIR__ . '/../smarty/libs/Autoloader.php';
    Smarty_Autoloader::register(true);
} elseif (is_file(__DIR__ . '/../libs/Autoloader.php')) {
    require_once __DIR__ . '/../libs/Autoloader.php';
    Smarty_Autoloader::register(true);
} elseif (is_file(__DIR__ . '/../smarty/libs/SmartyBC.class.php')) {
    require_once __DIR__ . '/../smarty/libs/SmartyBC.class.php';
} elseif (is_file(__DIR__ . '/../libs/SmartyBC.class.php')) {
    require_once __DIR__ . '/../libs/SmartyBC.class.php';
} else {
    throw new Exception('can not locate Smarty distribution');
}
if (!defined('SMARTY_COMPOSER_INSTALL')) {
    foreach (array(__DIR__ . '/../../autoload.php', __DIR__ . '/../vendor/autoload.php',
                 __DIR__ . '/vendor/autoload.php') as $file) {
        if (file_exists($file)) {
            define('SMARTY_COMPOSER_INSTALL', $file);
            break;
        }
    }
    unset($file);
}
if (!class_exists('PHPUnit_Framework_TestCase')) {
    require_once SMARTY_COMPOSER_INSTALL;
}

require_once 'PHPUnit_Smarty.php';
if (!ini_get('date.timezone')) {
    ini_set('date.timezone', 'UTC');
}



