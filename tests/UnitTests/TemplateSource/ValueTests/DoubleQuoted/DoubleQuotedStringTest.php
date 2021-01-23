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
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("../../../__shared/templates/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test double qouted strings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestDoubleQuoted
     */
    public function testDoubleQuoted($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "testDoubleQuotes_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('bar', 'buh');
        $this->assertEquals($result, $this->smarty->fetch($file),
                            "testDoubleQuoted - {$code} - {$name}");
    }

    /*
      * Data provider für testDoubleQuoted
      */
    public function dataTestDoubleQuoted()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array('{$foo="Hello World"}{$foo}', 'Hello World', 'simple', $i ++),
                     array('{$bar=1}{$foo="Hello {$bar+2} World"}{$foo}', 'Hello 3 World', 'withExpression', $i ++),
                     array('{$bar=\'blah\'}{$foo="Hello $bar World"}{$foo}', 'Hello blah World', 'withVariable', $i ++),
                     array('{$bar=\'blah\'}{$buh=\'wow\'}{$foo="Hello $bar$buh World"}{$foo}', 'Hello blahwow World', 'with2Variables', $i ++),
                     array('{$bar=\'blah\'}{$foo="Hello `$bar`.test World"}{$foo}', 'Hello blah.test World', 'withVarBacktick', $i ++),
                     array('{$bar=\'blah\'}{$buh=\'buh\'}{$foo="Hello `$bar``$buh`.test World"}{$foo}', 'Hello blahbuh.test World', 'with2VarBacktick', $i ++),
                     array('{$barbuh=\'blah\'}{$buh=\'buh\'}{$foo="Hello `$bar{$buh}`.test World"}{$foo}', 'Hello blah.test World', 'withVariableVarBacktick', $i ++),
                     array('{$bar[1][2]=\'blah\'}{$foo="Hello `$bar.1.2`.test World"}{$foo}', 'Hello blah.test World', 'withVarIndexSmartyBacktick', $i ++),
                     array('{$bar[1][2]=\'blah\'}{$foo="Hello `$bar[1][2]`.test World"}{$foo}', 'Hello blah.test World', 'withVarIndexPhPBacktick', $i ++),
                     array('{$a=1}{"`$a+1`"}', '2', 'withExpressionBacktick', $i ++),
                     array('{$foo="Hello {counter start=3} World"}{$foo}', 'Hello 3 World', 'withCounterTag', $i ++),
                     array('{$foo="Hello {counter start=2}{counter} World"}{$foo}', 'Hello 23 World', 'with2CounterTag', $i ++),
                     array('{$x=1}{$y=2}{$z=true}{"Hello{if $z} {$x} {else}{$y}{/if}World"}', 'Hello 1 World', 'withIfTag', $i ++),
                     array('{$bar=\'blah\'}{$foo="Hello {$bar}.test World"}{$foo}', 'Hello blah.test World', 'withDelimiter', $i ++),
                     array('{$foo="Hello \" World"}{$foo}', 'Hello " World', 'escaped', $i ++),
                     array('{$foo="Hello \'World\'"}{$foo}', 'Hello \'World\'', 'withSingleQuotes', $i ++),
                     array('{$foo="Hello {\'World\'} Test"}{$foo}', 'Hello World Test', 'withSingleQuoteTag', $i ++),
                     array('{$foo=""}{$foo}', '', 'empty', $i ++),
        );
    }


    /**
     * test unclosed block tag
     */
    public function testDoubleQuotedUnclosedBlock_001()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('unclosed \'{if}\' in doubled quoted string');
        $this->smarty->fetch('001_unclosedBlock.tpl');
    }

    /**
     *
     * test closed block tag
     * {"{if true}hello world{/if}"}
     *
     */
    public function testDoubleQuotedClosedBlock_001()
    {
        $this->assertEquals('hello world', $this->smarty->fetch('001_closedBlock.tpl'));
    }

}
