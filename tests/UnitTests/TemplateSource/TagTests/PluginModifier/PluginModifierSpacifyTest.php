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
class PluginModifierSpacifyTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
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
