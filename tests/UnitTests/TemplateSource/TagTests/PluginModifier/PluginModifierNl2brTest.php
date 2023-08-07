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
class PluginModifierNl2brTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('string:{$v|nl2br}');
        $tpl->assign("v", "Line1\nLine2");
        $this->assertEquals("Line1<br />\nLine2", $this->smarty->fetch($tpl));
    }

    public function testNoXHTML()
    {
        $tpl = $this->smarty->createTemplate('string:{$v|nl2br:false}');
        $tpl->assign("v", "Line1\nLine2");
        $this->assertEquals("Line1<br>\nLine2", $this->smarty->fetch($tpl));
    }
}
