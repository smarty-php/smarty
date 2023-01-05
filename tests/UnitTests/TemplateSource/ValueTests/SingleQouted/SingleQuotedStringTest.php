<?php
/**
 * Smarty PHPunit tests single quoted strings
 *

 * @author  Uwe Tews
 */

/**
 * class for single quoted string tests
 *
 * 
 * 
 * 
 */
class SingleQuotedStringTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test simple single quoted string
     */
    public function testSimpleSingleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'Hello World\'}{$foo}');
        $this->assertEquals('Hello World', $this->smarty->fetch($tpl));
    }

    /**
     * test that tags not interpreted in single quoted strings
     */
    public function testTagsInSingleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'Hello {1+2} World\'}{$foo}');
        $this->assertEquals('Hello {1+2} World', $this->smarty->fetch($tpl));
    }

    /**
     * test that vars not interpreted in single quoted strings
     */
    public function testVarsInSingleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'Hello $bar World\'}{$foo}');
        $this->assertEquals('Hello $bar World', $this->smarty->fetch($tpl));
    }

    /**
     * test double quotes in single quoted strings
     */
    public function testDoubleQuotesInSingleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'Hello "World"\'}{$foo}');
        $this->assertEquals('Hello "World"', $this->smarty->fetch($tpl));
    }

    /**
     * test escaped single quotes in single quoted strings
     */
    public function testEscapedSingleQuotesInSingleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'Hello \\\'World\'}{$foo}');
        $this->assertEquals("Hello 'World", $this->smarty->fetch($tpl));
    }

    /**
     * test empty single quoted strings
     */
    public function testEmptySingleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'\'}{$foo}');
        $this->assertEquals("", $this->smarty->fetch($tpl));
    }
}
