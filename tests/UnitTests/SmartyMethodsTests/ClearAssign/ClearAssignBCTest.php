<?php
/**
 * Smarty PHPunit tests clearing assigned variables
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for clearing assigned variables tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class ClearAssignBCTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public $loadSmarty = false;
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));

        $this->smartyBC->assign('foo', 'foo');
        $this->smartyBC->assign('bar', 'bar');
        $this->smartyBC->assign('blar', 'blar');
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    public function testSmarty2ClearAssign()
    {
        $this->smartyBC->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE | E_WARNING));
        $this->smartyBC->clear_assign('blar');
        $this->assertEquals('foobar', $this->smartyBC->fetch('eval:{$foo}{$bar}{$blar}'));
    }

    public function testSmarty2ArrayClearAssign()
    {
        $this->smartyBC->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE | E_WARNING));
        $this->smartyBC->clear_assign(array('blar', 'foo'));
        $this->assertEquals('bar', $this->smartyBC->fetch('eval:{$foo}{$bar}{$blar}'));
    }
}
