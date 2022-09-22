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
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class PluginModifierCountCharactersTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testDefault()
    {
        $result = "29";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wave Linked to Temperatures."|count_characters}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testDefaultWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $this->smarty->setCompileId ('mb');
        $result = "29";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wave Linked to Temperatures."|count_characters}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testSpaces()
    {
        $result = "33";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wave Linked to Temperatures."|count_characters:true}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testSpacesWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $this->smarty->setCompileId ('mb');
        $result = "33";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wave Linked to Temperatures."|count_characters:true}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testUmlauts()
    {
        $result = "29";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wäve Linked tö Temperatures."|count_characters}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlautsWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $this->smarty->setCompileId ('mb');
        $result = "29";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wäve Linked tö Temperatures."|count_characters}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testUmlautsSpaces()
    {
        $result = "33";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wäve Linked tö Temperatures."|count_characters:true}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlautsSpacesWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $this->smarty->setCompileId ('mb');
        $result = "33";
        $tpl = $this->smarty->createTemplate('string:{"Cold Wäve Linked tö Temperatures."|count_characters:true}');
        $this->assertNotEquals($result, $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }
}
