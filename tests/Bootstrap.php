<?php
/*
* This file is part of the Smarty PHPUnit tests.
*
*/
/*
 * Smarty PHPUnit Bootstrap
 */
require_once dirname(__FILE__) . '/Config.php';
require_once dirname(__FILE__) . '/../vendor/autoload.php';
require_once dirname(__FILE__) . '/../libs/bootstrap.php';

if (!class_exists('\PHPUnit_Framework_TestCase') && class_exists('\PHPUnit\Framework\TestCase')) {
    class_alias('\PHPUnit\Framework\TestCase', '\PHPUnit_Framework_TestCase');
    class_alias('\PHPUnit\Framework\Error\Notice', '\PHPUnit_Framework_Error_Notice');
    class_alias('\PHPUnit\Framework\Error\Error', '\PHPUnit_Framework_Error_Error');
    class_alias('\PHPUnit\Framework\Error\Warning', '\PHPUnit_Framework_Error_Warning');
    class_alias('\PHPUnit\Framework\Error\Warning', '\PHPUnit_Framework_Error_Deprecated');
}

require_once 'PHPUnit_Smarty.php';
if (!ini_get('date.timezone')) {
    ini_set('date.timezone', 'Europe/Berlin');
}



