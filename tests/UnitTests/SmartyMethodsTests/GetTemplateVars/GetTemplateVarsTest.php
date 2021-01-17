<?php
/**
 * Smarty PHPunit tests getTemplateVars method
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for getTemplateVars method test
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class GetTemplateVarsTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test root getTemplateVars single value
     */
    public function testGetSingleTemplateVarScopeRoot()
    {
        $this->smarty->assign('foo', 'bar');
        $this->smarty->assign('blar', 'buh');
        $this->assertEquals("bar", $this->smarty->getTemplateVars('foo'));
    }

    /**
     * test root getTemplateVars all values
     */
    public function testGetAllTemplateVarsScopeRoot()
    {
        $this->smarty->assign('foo', 'bar');
        $this->smarty->assign('blar', 'buh');
        $vars = $this->smarty->getTemplateVars();
        $this->assertTrue(is_array($vars));
        $this->assertEquals("bar", $vars['foo']);
        $this->assertEquals("buh", $vars['blar']);
    }

    /**
     * test single variable with data object chain
     */
    public function testGetSingleTemplateVarScopeAll()
    {
        $data1 = $this->smarty->createData($this->smarty);
        $data2 = $this->smarty->createData($data1);
        $this->smarty->assign('foo', 'bar');
        $this->smarty->assign('blar', 'buh');
        $this->assertEquals("bar", $this->smarty->getTemplateVars('foo', $data2));
    }

    /**
     * test get all variables with data object chain
     */
    public function testGetAllTemplateVarsScopeAll()
    {
        $data1 = $this->smarty->createData($this->smarty);
        $data2 = $this->smarty->createData($data1);
        $this->smarty->assign('foo', 'bar');
        $data1->assign('blar', 'buh');
        $data2->assign('foo2', 'bar2');
        $vars = $this->smarty->getTemplateVars(null, $data2);
        $this->assertTrue(is_array($vars));
        $this->assertEquals("bar", $vars['foo']);
        $this->assertEquals("bar2", $vars['foo2']);
        $this->assertEquals("buh", $vars['blar']);
    }

    /**
     * test get all variables with data object chain search parents disabled
     */
    public function testGetAllTemplateVarsScopeAllNoParents()
    {
        $data1 = $this->smarty->createData($this->smarty);
        $data2 = $this->smarty->createData($data1);
        $this->smarty->assign('foo', 'bar');
        $data1->assign('blar', 'buh');
        $data2->assign('foo2', 'bar2');
        $vars = $this->smarty->getTemplateVars(null, $data2, false);
        $this->assertTrue(is_array($vars));
        $this->assertFalse(isset($vars['foo']));
        $this->assertEquals("bar2", $vars['foo2']);
        $this->assertFalse(isset($vars['blar']));
    }

    /**
     * test get single variables with data object chain search parents disabled
     */
    public function testGetSingleTemplateVarsScopeAllNoParents()
    {
        error_reporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
        $data1 = $this->smarty->createData($this->smarty);
        $data2 = $this->smarty->createData($data1);
        $this->smarty->assign('foo', 'bar');
        $data1->assign('blar', 'buh');
        $data2->assign('foo2', 'bar2');
        $this->assertEquals("", $this->smarty->getTemplateVars('foo', $data2, false));
        $this->assertEquals("bar2", $this->smarty->getTemplateVars('foo2', $data2, false));
        $this->assertEquals("", $this->smarty->getTemplateVars('blar', $data2, false));
    }
}
