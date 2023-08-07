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
class PluginModifierCountWordsTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Dealers Will Hear Car Talk at Noon."|count_words}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testDashes()
    {
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Smalltime-Dealers Will Hear Car Talk at Noon."|count_words}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlauts()
    {
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Dealers Will Hear Cär Talk at Nöön."|count_words}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

}
