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
class PluginModifierStripTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('string:{" hello     spaced words  "|strip}');
        $this->assertEquals(" hello spaced words ", $this->smarty->fetch($tpl));
    }

    public function testUnicodeSpaces()
    {
        // Some Unicode Spaces
        $string = "&#8199;hello      spaced&#8196; &#8239;  &#8197;&#8199;  words  ";
        $string = html_entity_decode($string, ENT_QUOTES | ENT_SUBSTITUTE | ENT_HTML401, 'UTF-8');
        $tpl = $this->smarty->createTemplate('string:{"' . $string . '"|strip}');
        $this->assertEquals(" hello spaced words ", $this->smarty->fetch($tpl));
    }

    public function testLinebreak()
    {
        $tpl = $this->smarty->createTemplate('string:{" hello
            spaced words  "|strip}');
        $this->assertEquals(" hello spaced words ", $this->smarty->fetch($tpl));
    }
}
