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
class ClearAssignTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->smarty->assign('blar', 'blar');
    }

    /**
     * test all variables accessable
     */
    public function testAllVariablesAccessable()
    {
        $this->assertEquals('foobarblar', $this->smarty->fetch('eval:{$foo}{$bar}{$blar}'));
    }

    /**
     * test simple clear assign
     */
    public function testClearAssign()
    {
        $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE | E_WARNING));
        $this->smarty->clearAssign('blar');
        $this->assertEquals('foobar', $this->smarty->fetch('eval:{$foo}{$bar}{$blar}'));
    }

    /**
     * test clear assign array of variables
     */
    public function testArrayClearAssign()
    {
        $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE | E_WARNING));
        $this->smarty->clearAssign(array('blar', 'foo'));
        $this->assertEquals('bar', $this->smarty->fetch('eval:{$foo}{$bar}{$blar}'));
    }
}
