<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginModifierLowerTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $result = "two convicts evade noose, jury hung.";
        $tpl = $this->smarty->createTemplate('string:{"Two Convicts Evade Noose, Jury Hung."|lower}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlauts()
    {
        $result = "two convicts eväde nööse, jury hung.";
        $tpl = $this->smarty->createTemplate('string:{"Two Convicts Eväde NöÖse, Jury Hung."|lower}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

}
