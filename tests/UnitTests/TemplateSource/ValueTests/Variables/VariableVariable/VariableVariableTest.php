<?php
/**
 * Smarty PHPunit tests variable variables
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for variable variables tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class VariableVariableTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test variable name in variable
     */
    public function testVariableVariable1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'bar\'}{$bar=123}{${$foo}}');
        $this->assertEquals('123', $this->smarty->fetch($tpl));
    }

    /**
     * test part of variable name in variable
     */
    public function testVariableVariable2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'a\'}{$bar=123}{$b{$foo}r}');
        $this->assertEquals('123', $this->smarty->fetch($tpl));
    }

    /**
     * test several parts of variable name in variable
     */
    public function testVariableVariable3()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'a\'}{$foo2=\'r\'}{$bar=123}{$b{$foo}{$foo2}}');
        $this->assertEquals('123', $this->smarty->fetch($tpl));
    }

    /**
     * test nesed parts of variable name in variable
     */
    public function testVariableVariable4()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'ar\'}{$foo2=\'oo\'}{$bar=123}{$b{$f{$foo2}}}');
        $this->assertEquals('123', $this->smarty->fetch($tpl));
    }
}
