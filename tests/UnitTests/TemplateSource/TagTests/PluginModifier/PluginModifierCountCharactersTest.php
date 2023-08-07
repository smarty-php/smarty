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
 * @preserveGlobalState    disabled
 * 
 */
class PluginModifierCountCharactersTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $result = "29";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wave Linked to Temperatures."|count_characters}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testSpaces()
    {
        $result = "33";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wave Linked to Temperatures."|count_characters:true}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlauts()
    {
        $result = "29";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wäve Linked tö Temperatures."|count_characters}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlautsSpaces()
    {
        $result = "33";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wäve Linked tö Temperatures."|count_characters:true}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

}
