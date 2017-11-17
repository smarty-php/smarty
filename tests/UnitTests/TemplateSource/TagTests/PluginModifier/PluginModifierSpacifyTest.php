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
class PluginModifierSpacifyTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testDefault()
    {
        $result = 'h e l l o   w ö r l d';
        $tpl = $this->smarty->createTemplate('string:{"hello wörld"|spacify}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testCharacter()
    {
        $result = 'h##e##l##l##o## ##w##ö##r##l##d';
        $tpl = $this->smarty->createTemplate('string:{"hello wörld"|spacify:"##"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }
}
