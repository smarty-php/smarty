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
class ClearAllAssignBCTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public $loadSmarty = false;
    protected $_dataBC = null;
    protected $_tplBC = null;

    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));

        $this->smartyBC->assign('foo', 'foo');
        $this->_dataBC = $this->smartyBC->createData($this->smartyBC);
        $this->_dataBC->assign('bar', 'bar');
        $this->_tplBC = $this->smartyBC->createTemplate('eval:{$foo}{$bar}{$blar}', null, null, $this->_dataBC);
        $this->_tplBC->assign('blar', 'blar');
    }

    public function testSmarty2ClearAllAssignInSmarty()
    {
        error_reporting((error_reporting() & ~(E_NOTICE | E_WARNING | E_USER_NOTICE)));
        $this->smartyBC->clear_all_assign();
        $this->assertEquals('barblar', $this->smartyBC->fetch($this->_tplBC));
    }
}
