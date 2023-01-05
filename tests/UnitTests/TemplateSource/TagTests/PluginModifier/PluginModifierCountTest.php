<?php
/**
 * Smarty PHPunit tests of modifier
 */

/**
 * class for modifier tests
 *
 * 
 * @preserveGlobalState    disabled
 * 
 */
class PluginModifierCountTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testArray()
    {
        $tpl = $this->smarty->createTemplate('string:count:{$v|count}');
        $tpl->assign("v", array(1, 2, 3));
        $this->assertEquals("count:3", $this->smarty->fetch($tpl));
    }

    public function testEmptyArray()
    {
        $tpl = $this->smarty->createTemplate('string:count:{$v|count}');
        $tpl->assign("v", array());
        $this->assertEquals("count:0", $this->smarty->fetch($tpl));
    }

    public function testNull()
    {
        $tpl = $this->smarty->createTemplate('string:count:{$v|count}');
        $tpl->assign("v", null);
        $this->assertEquals("count:0", $this->smarty->fetch($tpl));
    }

    public function testString()
    {
        $tpl = $this->smarty->createTemplate('string:count:{$v|count}');
        $tpl->assign("v", "string");
        $this->assertEquals("count:1", $this->smarty->fetch($tpl));
    }

}
