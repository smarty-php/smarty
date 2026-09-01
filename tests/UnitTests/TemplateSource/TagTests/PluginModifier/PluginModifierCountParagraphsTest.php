<?php
/**
 * Smarty PHPunit tests of modifier
 */

/**
 * class for modifier tests
 *
 * 
 * 
 * 
 */
class PluginModifierCountParagraphsTest extends PHPUnit_Smarty
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
    $tpl = $this->smarty->createTemplate('string:{"One paragraph only."|count_paragraphs}');
    $this->assertEquals("1", $this->smarty->fetch($tpl));
    $tpl = $this->smarty->createTemplate('string:{"First line.\nSecond line."|count_paragraphs}');
    $this->assertEquals("2", $this->smarty->fetch($tpl));
    $tpl = $this->smarty->createTemplate('string:{"Paragraph1\nParagraph2\nParagraph3"|count_paragraphs}');
    $this->assertEquals("3", $this->smarty->fetch($tpl));
    $tpl = $this->smarty->createTemplate('string:{"Paragraph1\n\nParagraph2"|count_paragraphs}');
    $this->assertEquals("2", $this->smarty->fetch($tpl));
    $tpl = $this->smarty->createTemplate('string:{"Paragraph1\r\nParagraph2\nParagraph3\rParagraph4"|count_paragraphs}');
    $this->assertEquals("4", $this->smarty->fetch($tpl));
    $tpl = $this->smarty->createTemplate('string:{"End with newline\n"|count_paragraphs}');
    $this->assertEquals("2", $this->smarty->fetch($tpl));
    $tpl = $this->smarty->createTemplate('string:{"\nStart with newline"|count_paragraphs}');
    $this->assertEquals("2", $this->smarty->fetch($tpl));
    }

    public function testNull()
    {
        $tpl = $this->smarty->createTemplate('string:{null|count_paragraphs}');
        $this->assertEquals("1", $this->smarty->fetch($tpl));
    }
}
