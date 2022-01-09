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
class PluginFunctionMailtoTest extends PHPUnit_Smarty
{
     public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testDefault()
    {
        $result = '<a href="mailto:me@example.com" >me@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testDefaultWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<a href="mailto:me@example.com" >me@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testText()
    {
        $result = '<a href="mailto:me@example.com" >send me some mail</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" text="send me some mail"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testTextWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<a href="mailto:me@example.com" >send me some mail</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" text="send me some mail"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testEncodeJavascript()
    {
        $result = '<script type="text/javascript">document.write(unescape(\'%3c%61%20%68%72%65%66%3d%22%6d%61%69%6c%74%6f%3a%6d%65%40%65%78%61%6d%70%6c%65%2e%63%6f%6d%22%20%3e%6d%65%40%65%78%61%6d%70%6c%65%2e%63%6f%6d%3c%2f%61%3e\'))</script>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" encode="javascript"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testEncodeJavascriptWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<script type="text/javascript">document.write(unescape(\'%3c%61%20%68%72%65%66%3d%22%6d%61%69%6c%74%6f%3a%6d%65%40%65%78%61%6d%70%6c%65%2e%63%6f%6d%22%20%3e%6d%65%40%65%78%61%6d%70%6c%65%2e%63%6f%6d%3c%2f%61%3e\'))</script>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" encode="javascript"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testEncodeJavascriptCharcode()
    {
        $result = '<script type="text/javascript">document.write(String.fromCharCode(60,97,32,104,114,101,102,61,34,109,97,105,108,116,111,58,109,101,64,101,120,97,109,112,108,101,46,99,111,109,34,32,62,109,101,64,101,120,97,109,112,108,101,46,99,111,109,60,47,97,62))</script>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" encode="javascript_charcode"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testEncodeJavascriptCharcodeWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<script type="text/javascript">document.write(String.fromCharCode(60,97,32,104,114,101,102,61,34,109,97,105,108,116,111,58,109,101,64,101,120,97,109,112,108,101,46,99,111,109,34,32,62,109,101,64,101,120,97,109,112,108,101,46,99,111,109,60,47,97,62))</script>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" encode="javascript_charcode"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testEncodeHex()
    {
        $result = '<a href="&#109;&#97;&#105;&#108;&#116;&#111;&#58;%6d%65@%65%78%61%6d%70%6c%65.%63%6f%6d" >&#x6d;&#x65;&#x40;&#x65;&#x78;&#x61;&#x6d;&#x70;&#x6c;&#x65;&#x2e;&#x63;&#x6f;&#x6d;</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" encode="hex"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testEncodeHexWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<a href="&#109;&#97;&#105;&#108;&#116;&#111;&#58;%6d%65@%65%78%61%6d%70%6c%65.%63%6f%6d" >&#x6d;&#x65;&#x40;&#x65;&#x78;&#x61;&#x6d;&#x70;&#x6c;&#x65;&#x2e;&#x63;&#x6f;&#x6d;</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" encode="hex"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testSubject()
    {
        $result = '<a href="mailto:me@example.com?subject=Hello%20to%20you%21" >me@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" subject="Hello to you!"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testSubjectWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<a href="mailto:me@example.com?subject=Hello%20to%20you%21" >me@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" subject="Hello to you!"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testCc()
    {
        $result = '<a href="mailto:me@example.com?cc=you@example.com,they@example.com" >me@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" cc="you@example.com,they@example.com"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testCcWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<a href="mailto:me@example.com?cc=you@example.com,they@example.com" >me@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" cc="you@example.com,they@example.com"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testExtra()
    {
        $result = '<a href="mailto:me@example.com" class="email">me@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" extra=\'class="email"\'}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testExtraWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<a href="mailto:me@example.com" class="email">me@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me@example.com" extra=\'class="email"\'}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testUmlauts()
    {
        $result = '<a href="mailto:me+smtpext@example.com?cc=you@example.com,they@example.com&subject=h%C3%A4llo%20w%C3%B6rld" >me+smtpext@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me+smtpext@example.com" cc="you@example.com,they@example.com" subject="hällo wörld"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
    }

    public function testUmlautsWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = '<a href="mailto:me+smtpext@example.com?cc=you@example.com,they@example.com&subject=h%C3%A4llo%20w%C3%B6rld" >me+smtpext@example.com</a>';
        $tpl = $this->smarty->createTemplate('eval:{mailto address="me+smtpext@example.com" cc="you@example.com,they@example.com" subject="hällo wörld"}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }
}
