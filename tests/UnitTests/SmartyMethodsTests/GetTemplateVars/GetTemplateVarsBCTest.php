<?php
/**
 * Smarty PHPunit tests get_template_vars method
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for get_template_vars method test
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class get_template_varsBCTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public $loadSmarty = false;

    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test root get_template_vars single value
     */
    public function testGetSingleTemplateVarScopeRoot()
    {
        $this->smartyBC->assign('foo', 'bar');
        $this->smartyBC->assign('blar', 'buh');
        $this->assertEquals("bar", $this->smartyBC->get_template_vars('foo'));
    }

    /**
     * test root get_template_vars all values
     */
    public function testGetAllTemplateVarsScopeRoot()
    {
        $this->smartyBC->assign('foo', 'bar');
        $this->smartyBC->assign('blar', 'buh');
        $vars = $this->smartyBC->get_template_vars();
        $this->assertTrue(is_array($vars));
        $this->assertEquals("bar", $vars['foo']);
        $this->assertEquals("buh", $vars['blar']);
    }

}
