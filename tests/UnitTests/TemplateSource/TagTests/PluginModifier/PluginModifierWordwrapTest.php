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
class PluginModifierWordwrapTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('string:{"Blind woman gets new kidney from dad she hasn\'t seen in years."|wordwrap}');
        $this->assertEquals("Blind woman gets new kidney from dad she hasn't seen in years.", $this->smarty->fetch($tpl));
    }

    public function testDefaultUmlauts()
    {
        $tpl = $this->smarty->createTemplate('string:{"äöüßñ woman ñsä new kidney from dad she hasn\'t seen in years."|wordwrap:30}');
        $this->assertEquals("äöüßñ woman ñsä new kidney\nfrom dad she hasn't seen in\nyears.", $this->smarty->fetch($tpl));
    }

    public function testLength()
    {
        $tpl = $this->smarty->createTemplate('string:{"Blind woman gets new kidney from dad she hasn\'t seen in years."|wordwrap:30}');
        $this->assertEquals("Blind woman gets new kidney\nfrom dad she hasn't seen in\nyears.", $this->smarty->fetch($tpl));
    }

    public function testBreak()
    {
        $tpl = $this->smarty->createTemplate('string:{"Blind woman gets new kidney from dad she hasn\'t seen in years."|wordwrap:30:"<br />\n"}');
        $this->assertEquals("Blind woman gets new kidney<br />\nfrom dad she hasn't seen in<br />\nyears.", $this->smarty->fetch($tpl));
    }

    public function testLong()
    {
        $tpl = $this->smarty->createTemplate('string:{"Blind woman withaverylongandunpronoucablenameorso gets new kidney from dad she hasn\'t seen in years."|wordwrap:26:"\n"}');
        $this->assertEquals("Blind woman\nwithaverylongandunpronoucablenameorso\ngets new kidney from dad\nshe hasn't seen in years.", $this->smarty->fetch($tpl));
    }

    public function testLongUmlauts()
    {
        $tpl = $this->smarty->createTemplate('string:{"äöüßñ woman ñsääöüßñameorsoäöüßñäöüßñäöüßñäöüßñßñ gets new kidney from dad she hasn\'t seen in years."|wordwrap:26}');
        $this->assertEquals("äöüßñ woman\nñsääöüßñameorsoäöüßñäöüßñäöüßñäöüßñßñ\ngets new kidney from dad\nshe hasn't seen in years.", $this->smarty->fetch($tpl));
    }

    public function testLongCut()
    {
        $tpl = $this->smarty->createTemplate('string:{"Blind woman withaverylongandunpronoucablenameorso gets new kidney from dad she hasn\'t seen in years."|wordwrap:26:"\n":true}');
        $this->assertEquals("Blind woman\nwithaverylongandunpronouca\nblenameorso gets new\nkidney from dad she hasn't\nseen in years.", $this->smarty->fetch($tpl));
    }

    public function testLongCutUmlauts()
    {
        $tpl = $this->smarty->createTemplate('string:{"äöüßñ woman ñsääöüßñameorsoäöüßñäöüßñäöüßñäöüßñßñ gets new kidney from dad she hasn\'t seen in years."|wordwrap:26:"\n":true}');
        $this->assertEquals("äöüßñ woman\nñsääöüßñameorsoäöüßñäöüßñä\nöüßñäöüßñßñ gets new\nkidney from dad she hasn't\nseen in years.", $this->smarty->fetch($tpl));
    }

    public function testLinebreaks()
    {
        $tpl = $this->smarty->createTemplate('string:{"Blind woman\ngets new kidney from dad she hasn\'t seen in years."|wordwrap:30}');
        $this->assertEquals("Blind woman\ngets new kidney from dad she\nhasn't seen in years.", $this->smarty->fetch($tpl));
    }

    public function testLinebreaks2()
    {
        $tpl = $this->smarty->createTemplate('string:{"Blind woman
            gets
            new kidney from dad she hasn\'t seen in years."|wordwrap:30}');
        $this->assertEquals("Blind woman
            gets
            new kidney from\ndad she hasn't seen in years.", $this->smarty->fetch($tpl));
    }

}
