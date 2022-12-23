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



