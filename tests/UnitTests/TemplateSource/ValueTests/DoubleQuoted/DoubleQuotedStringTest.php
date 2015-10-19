<?php
/**
 * Smarty PHPunit tests double quoted strings
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for double quoted string tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
*/
class DoubleQuotedStringTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test simple double quoted string
     */
    public function testSimpleDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo="Hello World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello World', $this->smarty->fetch($tpl));
    }

    /**
     * test expression tags in double quoted strings
     */
    public function testTagsInDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo="Hello {1+2} World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello 3 World', $this->smarty->fetch($tpl));
    }

    /**
     * test vars in double quoted strings
     */
    public function testVarsInDoubleQuotedString1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$bar=\'blah\'}{$foo="Hello $bar World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello blah World', $this->smarty->fetch($tpl));
    }

    public function testVarsInDoubleQuotedString2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$bar=\'blah\'}{$buh=\'buh\'}{$foo="Hello $bar$buh World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello blahbuh World', $this->smarty->fetch($tpl));
    }

    /**
     * test vars with backtick in double quoted strings
     */
    public function testVarsBacktickInDoubleQuotedString1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$bar=\'blah\'}{$foo="Hello `$bar`.test World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello blah.test World', $this->smarty->fetch($tpl));
    }

    public function testVarsBacktickInDoubleQuotedString2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$bar=\'blah\'}{$buh=\'buh\'}{$foo="Hello `$bar``$buh`.test World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello blahbuh.test World', $this->smarty->fetch($tpl));
    }

    /**
     * test variable vars with backtick in double quoted strings
     */
    public function testVariableVarsBacktickInDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$barbuh=\'blah\'}{$buh=\'buh\'}{$foo="Hello `$bar{$buh}`.test World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello blah.test World', $this->smarty->fetch($tpl));
    }

    /**
     * test array vars with backtick in double quoted strings
     */
    public function testArrayVarsBacktickInDoubleQuotedString1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$bar[1][2]=\'blah\'}{$foo="Hello `$bar.1.2`.test World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello blah.test World', $this->smarty->fetch($tpl));
    }

    public function testArrayVarsBacktickInDoubleQuotedString2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$bar[1][2]=\'blah\'}{$foo="Hello `$bar[1][2]`.test World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello blah.test World', $this->smarty->fetch($tpl));
    }

    /**
     * test expression in backtick in double quoted strings
     */
    public function testExpressionBacktickInDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$a=1}{"`$a+1`"}', null, null, $this->smarty);
        $this->assertEquals('2', $this->smarty->fetch($tpl));
    }

    /**
     * test smartytag in double quoted strings
     */
    public function testSmartytagInDoubleQuotedString1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo="Hello {counter start=1} World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello 1 World', $this->smarty->fetch($tpl));
    }

    public function testSmartytagInDoubleQuotedString2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo="Hello {counter start=1}{counter} World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello 12 World', $this->smarty->fetch($tpl));
    }

    /**
     * test block smartytag in double quoted strings
     */
    public function testSmartyBlockTagInDoubleQuotedString1()
    {
        $this->smarty->assign('x', 1);
        $this->smarty->assign('y', 1);
        $this->smarty->assign('z', true);
        $this->assertEquals('Hello 1 World', $this->smarty->fetch('string:{"Hello{if $z} {$x} {else}{$y}{/if}World"}'));
    }

    /**
     * test vars in delimiter in double quoted strings
     */
    public function testVarsDelimiterInDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$bar=\'blah\'}{$foo="Hello {$bar}.test World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello blah.test World', $this->smarty->fetch($tpl));
    }

    /**
     * test escaped quotes in double quoted strings
     */
    public function testEscapedQuotesInDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo="Hello \" World"}{$foo}', null, null, $this->smarty);
        $this->assertEquals('Hello " World', $this->smarty->fetch($tpl));
    }

    /**
     * test single quotes in double quoted strings
     */
    public function testSingleQuotesInDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo="Hello \'World\'"}{$foo}', null, null, $this->smarty);
        $this->assertEquals("Hello 'World'", $this->smarty->fetch($tpl));
    }

    /**
     * test single quote tags in double quoted strings
     */
    public function testSingleQuoteTagsInDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo="Hello {\'World\'} Test"}{$foo}', null, null, $this->smarty);
        $this->assertEquals("Hello World Test", $this->smarty->fetch($tpl));
    }

    /**
     * test empty double quoted strings
     */
    public function testEmptyDoubleQuotedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=""}{$foo}', null, null, $this->smarty);
        $this->assertEquals("", $this->smarty->fetch($tpl));
    }
}
