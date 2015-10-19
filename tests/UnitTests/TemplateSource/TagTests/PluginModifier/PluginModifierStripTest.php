<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginModifierStripTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('eval:{" hello     spaced words  "|strip}');
        $this->assertEquals(" hello spaced words ", $this->smarty->fetch($tpl));
    }

    public function testUnicodeSpaces()
    {
        // Some Unicode Spaces
        $string = "&#8199;hello      spaced&#8196; &#8239;  &#8197;&#8199;  words  ";
        $string = mb_convert_encoding($string, 'UTF-8', "HTML-ENTITIES");
        $tpl = $this->smarty->createTemplate('eval:{"' . $string . '"|strip}');
        $this->assertEquals(" hello spaced words ", $this->smarty->fetch($tpl));
    }

    public function testLinebreak()
    {
        $tpl = $this->smarty->createTemplate('eval:{" hello
            spaced words  "|strip}');
        $this->assertEquals(" hello spaced words ", $this->smarty->fetch($tpl));
    }
}
