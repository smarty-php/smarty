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
class PluginModifierCountWordsTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testDefault()
    {
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Dealers Will Hear Car Talk at Noon."|count_words}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testDefaultWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Dealers Will Hear Car Talk at Noon."|count_words}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testDashes()
    {
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Smalltime-Dealers Will Hear Car Talk at Noon."|count_words}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testDashesWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Smalltime-Dealers Will Hear Car Talk at Noon."|count_words}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testUmlauts()
    {
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Dealers Will Hear Cär Talk at Nöön."|count_words}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlautsWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $result = "7";
        $tpl = $this->smarty->createTemplate('string:{"Dealers Will Hear Cär Talk at Nöön."|count_words}');
        $this->assertNotEquals($result, $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }
}
