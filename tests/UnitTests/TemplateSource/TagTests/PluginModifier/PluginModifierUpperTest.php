<?php
/**
 * Smarty PHPunit tests of modifier
 *

 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 * 
 * 
 *
 */
class PluginModifierUpperTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $result = "IF STRIKE ISN'T SETTLED QUICKLY IT MAY LAST A WHILE.";
        $tpl = $this->smarty->createTemplate('string:{"If Strike isn\'t Settled Quickly it may Last a While."|upper}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlauts()
    {
        $result = "IF STRIKE ISN'T SÄTTLED ÜQUICKLY IT MAY LAST A WHILE.";
        $tpl = $this->smarty->createTemplate('string:{"If Strike isn\'t Sättled ÜQuickly it may Last a While."|upper}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

}
