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
class PluginModifierCharsetTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testToLatin1()
    {
        $encoded = "hällö wörld 1";
        $result = utf8_decode($encoded);
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|to_charset}');
        $this->assertEquals(str_replace("\r", '', $result), $tpl->fetch());
    }

    public function testToLatin1WithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $this->smarty->setCompileId('mb');
        $encoded = "hällö wörld 2";
        $result = utf8_decode($encoded);
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|to_charset}');
        $this->assertEquals($encoded, $tpl->fetch());
        Smarty::$_MBSTRING = true;
    }

    public function testFromLatin1()
    {
        $result = "hällö wörld 3";
        $encoded = utf8_decode($result);
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|from_charset}');
        $this->assertEquals(str_replace("\r", '', $result), $tpl->fetch());
    }

    public function testFromLatin1WithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $this->smarty->setCompileId('mb');
        $result = "hällö wörld 4";
        $encoded = utf8_decode($result);
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|from_charset}');
        $this->assertEquals($encoded, $tpl->fetch());
        Smarty::$_MBSTRING = true;
    }

    public function testFromUtf32le()
    {
        $result = "hällö wörld 5";
        $encoded = mb_convert_encoding($result, "UTF-32LE", "UTF-8");
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|from_charset:"UTF-32LE"}');
        $this->assertEquals(str_replace("\r", '', $result), $tpl->fetch());
    }

    public function testFromUtf32leWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $this->smarty->setCompileId('mb');
        $result = "hällö wörld 6";
        $encoded = mb_convert_encoding($result, "UTF-32LE", "UTF-8");
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|from_charset:"UTF-32LE"}');
        $this->assertEquals($encoded, $tpl->fetch());
        Smarty::$_MBSTRING = true;
    }

    public function testToUtf32le()
    {
        $encoded = "hällö wörld 7";
        $result = mb_convert_encoding($encoded, "UTF-32LE", "UTF-8");
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|to_charset:"UTF-32LE"}');
        $this->assertEquals(str_replace("\r", '', $result), $tpl->fetch());
    }

    public function testToUtf32leWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $this->smarty->setCompileId('mb');
        $encoded = "hällö wörld 8";
        $result = mb_convert_encoding($encoded, "UTF-32LE", "UTF-8");
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|to_charset:"UTF-32LE"}');
        $this->assertEquals($encoded, $tpl->fetch());
        Smarty::$_MBSTRING = true;
    }
}
