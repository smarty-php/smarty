<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class ModifierTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test modifier
     *
     * @not                 runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestModifier
     */
    public function testModifier($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "testModifier_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('bar', 'buh');
        $this->assertEquals($result, $this->smarty->fetch($file),
                            "testModifier - {$code} - {$name}");
    }

    /*
      * Data provider für testModifier
      */
    public function dataTestModifier()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    */
        return array(array('{"hello world"|strlen}', '11', 'OnString', $i ++),
                     array('{$foo ="hello world"}{$foo|strlen}', '11', 'OnVar', $i ++),
                     array('{"hello world"|truncate:6}', 'hel...', 'TruncatePlugin', $i ++),
                     array('{$foo=7}{"hello world"|truncate:$foo}', 'hell...', 'TruncatePluginLengthVar', $i ++),
                     array('{$foo=10}{$bar=\'<>\'}{"hello world"|truncate:$foo:$bar}', 'hello<>', 'TruncatePluginAllVar', $i ++),
                     array('{"hello world"|truncate:6|strlen}', '6', 'Chain', $i ++),
                     array('{"hello world"|truncate:6:"xx"|cat:"Smarty"}', 'hellxxSmarty', 'ChainVar', $i ++),
                     array('{"hello world"|truncate:6|strlen}', '6', 'Chain', $i ++),
                     array('{if "hello world"|truncate:6|strlen == 6}okay{/if}', 'okay', 'InIF', $i ++),
                     array('{"hello world"|truncate:6|strlen + ("hello world"|truncate:8|strlen)}', '14', 'Expression', $i ++),
                     array('{1.1*7.1|round}', '7.7', 'InExpression', $i ++),
                     array('{counter|truncate:5 start=100000}', '10...', 'PluginOutput', $i ++),
                     array('{1 + [1,2,3]|count}', '4', 'SumExpression', $i ++),
       );
    }




    /**
     * test registered modifier static class
     */
    public function testModifierRegisteredStaticClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', array('testmodifierclass', 'staticcall'));
        $this->smarty->assign('foo', 1);
        $this->assertEquals("mymodifier static 1", $this->smarty->fetch('testModifier_RegisteredStatic.tpl'));
    }

    /**
     * test registered modifier method call
     */
    public function testModifierRegisteredMethodCall()
    {
        $obj = new testmodifierclass();
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', array($obj, 'method'));
        $this->smarty->assign('foo', 3);
        $this->assertEquals("mymodifier method 3", $this->smarty->fetch('testModifier_RegisteredMethod.tpl'));
    }

    /**
     * test unknown modifier error
     */
    public function testUnknownModifier()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('unknown modifier \'unknown\'');
        $this->smarty->fetch('eval:{"hello world"|unknown}');
    }

    /**
     * test default modifier
     */
    public function testDefaultModifier()
    {
        $this->smarty->default_modifiers = array('escape');
        $this->smarty->assign('foo', '<bar>');
        $this->assertEquals('&lt;bar&gt;<bar>', $this->smarty->fetch('testModifier_Default.tpl'));
    }
}

function testmodifier($value)
{
    return "mymodifier function $value";
}

class testmodifierclass
{
    static function staticcall($value)
    {
        return "mymodifier static $value";
    }

    public function method($value)
    {
        return "mymodifier method $value";
    }
}
