<?php
/**
 * Smarty PHPunit tests clearing all assigned variables
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for clearing all assigned variables tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class ClearAllAssignTest extends PHPUnit_Smarty
{
    protected $_data = null;
    protected $_tpl = null;

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);

        $this->smarty->assign('foo', 'foo');
        $this->_data = $this->smarty->createData($this->smarty);
        $this->_data->assign('bar', 'bar');
        $this->_tpl = $this->smarty->createTemplate('eval:{$foo}{$bar}{$blar}', null, null, $this->_data);
        $this->_tpl->assign('blar', 'blar');
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test all variables accessable
     */
    public function testAllVariablesAccessable()
    {
        $this->assertEquals('foobarblar', $this->smarty->fetch($this->_tpl));
    }

    /**
     * test clear all assign in template
     */
    public function testClearAllAssignInTemplate()
    {
        $this->smarty->setErrorReporting($this->smarty->error_reporting & ~(E_NOTICE | E_USER_NOTICE | E_WARNING));
        $this->_tpl->clearAllAssign();
        $this->assertEquals('foobar', $this->smarty->fetch($this->_tpl));
    }

    /**
     * test clear all assign in data
     */
    public function testClearAllAssignInData()
    {
        $this->smarty->setErrorReporting($this->smarty->error_reporting & ~(E_NOTICE | E_USER_NOTICE | E_WARNING));
        $this->_data->clearAllAssign();
        $this->assertEquals('fooblar', $this->smarty->fetch($this->_tpl));
    }

    /**
     * test clear all assign in Smarty object
     */
    public function testClearAllAssignInSmarty()
    {
        $this->smarty->setErrorReporting($this->smarty->error_reporting & ~(E_NOTICE | E_USER_NOTICE | E_WARNING));
        $this->smarty->clearAllAssign();
        $this->assertEquals('barblar', $this->smarty->fetch($this->_tpl));
    }
}
