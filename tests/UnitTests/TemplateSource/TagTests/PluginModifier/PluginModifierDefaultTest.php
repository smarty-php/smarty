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
class PluginModifierDefaultTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('string:{$array.a.b|default:$array.c:\'defaultval\'}');

        $this->smarty->assign('array', []);
        $this->assertEquals('defaultval', $this->smarty->fetch($tpl));
    }
}
