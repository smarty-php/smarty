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
class PluginModifierEscapeTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testHtml()
    {
        $tpl = $this->smarty->createTemplate('string:{"I\'m some <html> to ä be \"escaped\" or &copy;"|escape:"html"}');
        $this->assertEquals("I&#039;m some &lt;html&gt; to ä be &quot;escaped&quot; or &amp;copy;", $this->smarty->fetch($tpl));
    }

    public function testHtmlWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"I\'m some <html> to ä be \"escaped\" or &copy;"|escape:"html"}');
        $this->assertEquals("I&#039;m some &lt;html&gt; to ä be &quot;escaped&quot; or &amp;copy;", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testHtmlDouble()
    {
        $tpl = $this->smarty->createTemplate('string:{"I\'m some <html> to ä be \"escaped\" or &copy;"|escape:"html":null:false}');
        $this->assertEquals("I&#039;m some &lt;html&gt; to ä be &quot;escaped&quot; or &copy;", $this->smarty->fetch($tpl));
    }

    public function testHtmlDoubleWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"I\'m some <html> to ä be \"escaped\" or &copy;"|escape:"html":null:false}');
        $this->assertEquals("I&#039;m some &lt;html&gt; to ä be &quot;escaped&quot; or &copy;", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testHtmlall()
    {
        $tpl = $this->smarty->createTemplate('string:{"I\'m some <html> to ä be \"escaped\" or &copy;"|escape:"htmlall"}');
        $this->assertEquals("I&#039;m some &lt;html&gt; to &auml; be &quot;escaped&quot; or &amp;copy;", $this->smarty->fetch($tpl));
    }

    public function testHtmlallWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"I\'m some <html> to ä be \"escaped\" or &copy;"|escape:"htmlall"}');
        $this->assertEquals("I&#039;m some &lt;html&gt; to &auml; be &quot;escaped&quot; or &amp;copy;", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testHtmlallDouble()
    {
        $tpl = $this->smarty->createTemplate('string:{"I\'m some <html> to ä be \"escaped\" or &copy;"|escape:"htmlall":null:false}');
        $this->assertEquals("I&#039;m some &lt;html&gt; to &auml; be &quot;escaped&quot; or &copy;", $this->smarty->fetch($tpl));
    }

    public function testHtmlallDoubleWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"I\'m some <html> to ä be \"escaped\" or &copy;"|escape:"htmlall":null:false}');
        $this->assertEquals("I&#039;m some &lt;html&gt; to &auml; be &quot;escaped&quot; or &copy;", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testUrl()
    {
        $tpl = $this->smarty->createTemplate('string:{"http://some.encoded.com/url?parts#foo"|escape:"url"}');
        $this->assertEquals("http%3A%2F%2Fsome.encoded.com%2Furl%3Fparts%23foo", $this->smarty->fetch($tpl));
    }

    public function testUrlWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"http://some.encoded.com/url?parts#foo"|escape:"url"}');
        $this->assertEquals("http%3A%2F%2Fsome.encoded.com%2Furl%3Fparts%23foo", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testUrlpathinfo()
    {
        $tpl = $this->smarty->createTemplate('string:{"http://some.encoded.com/url?parts#foo"|escape:"urlpathinfo"}');
        $this->assertEquals("http%3A//some.encoded.com/url%3Fparts%23foo", $this->smarty->fetch($tpl));
    }

    public function testUrlpathinfoWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"http://some.encoded.com/url?parts#foo"|escape:"urlpathinfo"}');
        $this->assertEquals("http%3A//some.encoded.com/url%3Fparts%23foo", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testHex()
    {
        $tpl = $this->smarty->createTemplate('string:{"a/cäa"|escape:"hex"}');
        $this->assertEquals("%61%2f%63%c3%a4%61", $this->smarty->fetch($tpl));
    }

    public function testHexWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"a/cäa"|escape:"hex"}');
        $this->assertEquals("%61%2f%63%c3%a4%61", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testHexentity()
    {
        $q = "a&#228;&#1047;&#1076;&#1088;&#1072;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $r = html_entity_decode($q, ENT_NOQUOTES, 'UTF-8');
        $tpl = $this->smarty->createTemplate('string:{"' . $r . '"|escape:"hexentity"}');
        $this->assertEquals("&#x61;&#xE4;&#x417;&#x434;&#x440;&#x430;&#x432;&#x441;&#x441;&#x442;&#x432;&#x443;&#x439;&#x442;&#x435;", $this->smarty->fetch($tpl));

        $tpl = $this->smarty->createTemplate('string:{"abc"|escape:"hexentity"}');
        $this->assertEquals("&#x61;&#x62;&#x63;", $this->smarty->fetch($tpl));
    }

    public function testHexentityWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $q = "a&#228;&#1047;&#1076;&#1088;&#1072;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $r = html_entity_decode($q, ENT_NOQUOTES, 'UTF-8');
        $tpl = $this->smarty->createTemplate('string:{"' . $r . '"|escape:"hexentity"}');
        $this->assertNotEquals("&#x61;&#xE4;&#x417;&#x434;&#x440;&#x430;&#x432;&#x441;&#x441;&#x442;&#x432;&#x443;&#x439;&#x442;&#x435;", $this->smarty->fetch($tpl));

        $tpl = $this->smarty->createTemplate('string:{"abc"|escape:"hexentity"}');
        $this->assertEquals("&#x61;&#x62;&#x63;", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testDecentity()
    {
        $q = "a&#228;&#1047;&#1076;&#1088;&#1072;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $r = html_entity_decode($q, ENT_NOQUOTES, 'UTF-8');
        $tpl = $this->smarty->createTemplate('string:{"' . $r . '"|escape:"decentity"}');
        $this->assertEquals("&#97;&#228;&#1047;&#1076;&#1088;&#1072;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;", $this->smarty->fetch($tpl));

        $tpl = $this->smarty->createTemplate('string:{"abc"|escape:"decentity"}');
        $this->assertEquals("&#97;&#98;&#99;", $this->smarty->fetch($tpl));
    }

    public function testDecentityWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $q = "a&#228;&#1047;&#1076;&#1088;&#1072;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;";
        $r = html_entity_decode($q, ENT_NOQUOTES, 'UTF-8');
        $tpl = $this->smarty->createTemplate('string:{"' . $r . '"|escape:"decentity"}');
        $this->assertNotEquals("&#97;&#228;&#1047;&#1076;&#1088;&#1072;&#1074;&#1089;&#1089;&#1090;&#1074;&#1091;&#1081;&#1090;&#1077;", $this->smarty->fetch($tpl));

        $tpl = $this->smarty->createTemplate('string:{"abc"|escape:"decentity"}');
        $this->assertEquals("&#97;&#98;&#99;", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testJavascript()
    {
        $tpl = $this->smarty->createTemplate('string:{"var x = { foo : \"bar\n\" };"|escape:"javascript"}');
        $this->assertEquals("var x = { foo : \\\"bar\\n\\\" };", $this->smarty->fetch($tpl));
    }

    public function testJavascriptWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"var x = { foo : \"bar\n\" };"|escape:"javascript"}');
        $this->assertEquals("var x = { foo : \\\"bar\\n\\\" };", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testMail()
    {
        $tpl = $this->smarty->createTemplate('string:{"smarty@example.com"|escape:"mail"}');
        $this->assertEquals("smarty [AT] example [DOT] com", $this->smarty->fetch($tpl));
    }

    public function testMailWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"smarty@example.com"|escape:"mail"}');
        $this->assertEquals("smarty [AT] example [DOT] com", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testNonstd()
    {
        $tpl = $this->smarty->createTemplate('string:{"sma\'rty|»example«.com"|escape:"nonstd"}');
        $this->assertEquals("sma'rty|&#187;example&#171;.com", $this->smarty->fetch($tpl));
    }

    public function testNonstdWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;$this->smarty->setCompileId('mb');
        $tpl = $this->smarty->createTemplate('string:{"' . utf8_decode('sma\'rty@»example«.com') . '"|escape:"nonstd"}');
        $this->assertEquals("sma'rty@&#187;example&#171;.com", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }
}
