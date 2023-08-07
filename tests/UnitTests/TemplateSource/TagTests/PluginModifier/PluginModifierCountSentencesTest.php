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
class PluginModifierCountSentencesTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('string:{"hello world."|count_sentences}');
        $this->assertEquals("1", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('string:{"hello world. I\'m another? Sentence!"|count_sentences}');
        $this->assertEquals("3", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('string:{"hello world.wrong"|count_sentences}');
        $this->assertEquals("0", $this->smarty->fetch($tpl));
    }

    public function testUmlauts()
    {
        $tpl = $this->smarty->createTemplate('string:{"hello worldä."|count_sentences}');
        $this->assertEquals("1", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('string:{"hello worldü. ä\'m another? Sentence!"|count_sentences}');
        $this->assertEquals("3", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('string:{"hello worlä.ärong"|count_sentences}');
        $this->assertEquals("0", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('string:{"hello worlä.wrong"|count_sentences}');
        $this->assertEquals("0", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('string:{"hello world.ärong"|count_sentences}');
        $this->assertEquals("0", $this->smarty->fetch($tpl));
    }
}
