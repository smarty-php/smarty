<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginModifierUpperTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testDefault()
    {
        $result = "IF STRIKE ISN'T SETTLED QUICKLY IT MAY LAST A WHILE.";
        $tpl = $this->smarty->createTemplate('string:{"If Strike isn\'t Settled Quickly it may Last a While."|upper}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testDefaultWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $result = "IF STRIKE ISN'T SETTLED QUICKLY IT MAY LAST A WHILE.";
        $tpl = $this->smarty->createTemplate('string:{"If Strike isn\'t Settled Quickly it may Last a While."|upper}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testUmlauts()
    {
        $result = "IF STRIKE ISN'T SÄTTLED ÜQUICKLY IT MAY LAST A WHILE.";
        $tpl = $this->smarty->createTemplate('string:{"If Strike isn\'t Sättled ÜQuickly it may Last a While."|upper}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlautsWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $result = "IF STRIKE ISN'T SÄTTLED ÜQUICKLY IT MAY LAST A WHILE.";
        $tpl = $this->smarty->createTemplate('string:{"If Strike isn\'t Sättled ÜQuickly it may Last a While."|upper}');
        $this->assertNotEquals($result, $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }
}
