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
class PluginModifierUnescapeTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testHtml()
    {
        $encoded = "a&#228;&#1047;&#1076;&#1088;&#1072;&gt;&lt;&amp;amp;&auml;&#228;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $result = "a&#228;&#1047;&#1076;&#1088;&#1072;><&amp;&auml;&#228;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|unescape:"html"}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testHtmlWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $encoded = "a&#228;&#1047;&#1076;&#1088;&#1072;&gt;&lt;&amp;amp;&auml;&#228;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $result = "a&#228;&#1047;&#1076;&#1088;&#1072;><&amp;&auml;&#228;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|unescape:"html"}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testHtmlall()
    {
        $encoded = "a&#228;&#1047;&#1076;&#1088;&#1072;&gt;&lt;&amp;amp;&auml;&#228;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $result = "aäЗдра><&amp;ääвсствуйте";
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|unescape:"htmlall"}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testHtmlallWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $encoded = "a&#228;&#1047;&#1076;&#1088;&#1072;&gt;&lt;&amp;amp;&auml;&#228;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $result = "aäЗдра><&amp;ääвсствуйте";
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|unescape:"htmlall"}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testUrl()
    {
        $encoded = "a%C3%A4%D0%97%D0%B4%D1%80%D0%B0%3E%3C%26amp%3B%C3%A4%C3%A4%D0%B2%D1%81%D1%81%D1%82%D0%B2%3F%3D%2B%D1%83%D0%B9%D1%82%D0%B5";
        $result = "aäЗдра><&amp;ääвсств?=+уйте";
        $tpl = $this->smarty->createTemplate('string:{"' . $encoded . '"|unescape:"url"}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testCharset()
    {
        $tpl = $this->smarty->createTemplate("string:{'&#039;Stiff Opposition Expected to Casketless Funeral Plan&#039;'|unescape:'htmlall':'utf-8'}");
        $this->assertEquals("'Stiff Opposition Expected to Casketless Funeral Plan'", $this->smarty->fetch($tpl));
    }

}
