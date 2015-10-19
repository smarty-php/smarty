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

    public function setUp()
    {
        $this->setUpSmarty(__DIR__);

        $this->smarty->assign('foo', 'foo');
        $this->_data = new Smarty_Data($this->smarty);
        $this->_data->assign('bar', 'bar');
        $this->_tpl = $this->smarty->createTemplate('eval:{$foo}{$bar}{$blar}', null, null, $this->_data);
        $this->_tpl->assign('blar', 'blar');
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
        error_reporting((error_reporting() & ~(E_NOTICE | E_USER_NOTICE)));
        $this->_tpl->clearAllAssign();
        $this->assertEquals('foobar', $this->smarty->fetch($this->_tpl));
    }

    /**
     * test clear all assign in data
     */
    public function testClearAllAssignInData()
    {
        error_reporting((error_reporting() & ~(E_NOTICE | E_USER_NOTICE)));
        $this->_data->clearAllAssign();
        $this->assertEquals('fooblar', $this->smarty->fetch($this->_tpl));
    }

    /**
     * test clear all assign in Smarty object
     */
    public function testClearAllAssignInSmarty()
    {
        error_reporting((error_reporting() & ~(E_NOTICE | E_USER_NOTICE)));
        $this->smarty->clearAllAssign();
        $this->assertEquals('barblar', $this->smarty->fetch($this->_tpl));
    }
}
