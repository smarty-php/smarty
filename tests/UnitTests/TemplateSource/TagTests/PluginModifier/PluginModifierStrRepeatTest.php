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
class PluginModifierStrRepeatTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('string:{$v|str_repeat:2}');
        $tpl->assign("v", "foo");
        $this->assertEquals("foofoo", $this->smarty->fetch($tpl));
    }

    public function testZeroTimes()
    {
        $tpl = $this->smarty->createTemplate('string:{$v|str_repeat:0}');
        $tpl->assign("v", "foo");
        $this->assertEquals("", $this->smarty->fetch($tpl));
    }
}
