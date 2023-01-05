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
 * @preserveGlobalState    disabled
 * 
 */
class PluginModifierCharsetTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testToLatin1()
    {
        $encoded = "hällö wörld 1";
        $result = mb_convert_encoding($encoded, 'ISO-8859-1', 'UTF-8');
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|to_charset}');
        $this->assertEquals(str_replace("\r", '', $result), $tpl->fetch());
    }

    public function testFromLatin1()
    {
        $result = "hällö wörld 3";
        $encoded = mb_convert_encoding($result, 'ISO-8859-1', 'UTF-8');
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|from_charset}');
        $this->assertEquals(str_replace("\r", '', $result), $tpl->fetch());
    }

    public function testFromUtf32le()
    {
        $result = "hällö wörld 5";
        $encoded = mb_convert_encoding($result, "UTF-32LE", "UTF-8");
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|from_charset:"UTF-32LE"}');
        $this->assertEquals(str_replace("\r", '', $result), $tpl->fetch());
    }

    public function testToUtf32le()
    {
        $encoded = "hällö wörld 7";
        $result = mb_convert_encoding($encoded, "UTF-32LE", "UTF-8");
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|to_charset:"UTF-32LE"}');
        $this->assertEquals(str_replace("\r", '', $result), $tpl->fetch());
    }

}
