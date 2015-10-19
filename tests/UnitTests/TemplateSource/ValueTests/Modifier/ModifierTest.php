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
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->enableSecurity();
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test PHP function as modifier
     */
    public function testPHPFunctionModifier()
    {
        $this->smarty->security_policy->php_modifiers = array('strlen');
        $tpl = $this->smarty->createTemplate('eval:{"hello world"|strlen}');
        $this->assertEquals("11", $this->smarty->fetch($tpl));
    }

    public function testPHPFunctionModifier2()
    {
        $this->smarty->security_policy->php_modifiers = array('strlen');
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value="hello world"}{$foo|strlen}');
        $this->assertEquals("11", $this->smarty->fetch($tpl));
    }

    /**
     * test plugin as modifier
     */
    public function testPluginModifier()
    {
        $tpl = $this->smarty->createTemplate('eval:{"hello world"|truncate:6}');
        $this->assertEquals("hel...", $this->smarty->fetch($tpl));
    }

    /**
     * test plugin as modifier with variable
     */
    public function testPluginModifierVar()
    {
        $tpl = $this->smarty->createTemplate('eval:{"hello world"|truncate:$foo}');
        $tpl->assign('foo', 6);
        $this->assertEquals("hel...", $this->smarty->fetch($tpl));
    }

    public function testPluginModifierVar2()
    {
        $tpl = $this->smarty->createTemplate('eval:{"hello world"|truncate:$foo:"   "}');
        $tpl->assign('foo', 6);
        $this->assertEquals("hel   ", $this->smarty->fetch($tpl));
    }

    public function testPluginModifierVar3()
    {
        $tpl = $this->smarty->createTemplate('eval:{"hello world"|truncate:$foo:$bar}');
        $tpl->assign('foo', 6);
        $tpl->assign('bar', '   ');
        $this->assertEquals("hel   ", $this->smarty->fetch($tpl));
    }

    /**
     * test modifier chaining
     */
    public function testModifierChaining()
    {
        $this->smarty->security_policy->php_modifiers = array('strlen');
        $tpl = $this->smarty->createTemplate('eval:{"hello world"|truncate:6|strlen}');
        $this->assertEquals("6", $this->smarty->fetch($tpl));
    }

    /**
     * test modifier in {if}
     */
    public function testModifierInsideIf()
    {
        $this->smarty->security_policy->php_modifiers = array('strlen');
        $tpl = $this->smarty->createTemplate('eval:{if "hello world"|truncate:6|strlen == 6}okay{/if}');
        $this->assertEquals("okay", $this->smarty->fetch($tpl));
    }

    /**
     * test modifier in expressions
     */
    public function testModifierInsideExpression()
    {
        $this->smarty->security_policy->php_modifiers = array('strlen');
        $tpl = $this->smarty->createTemplate('eval:{"hello world"|truncate:6|strlen + ("hello world"|truncate:8|strlen)}');
        $this->assertEquals("14", $this->smarty->fetch($tpl));
    }

    public function testModifierInsideExpression2()
    {
        $this->smarty->security_policy->php_modifiers = array('round');
        $tpl = $this->smarty->createTemplate('eval:{1.1*7.1|round}');
        $this->assertEquals("7.7", $this->smarty->fetch($tpl));
    }

    /**
     * test modifier at plugin result
     */
    public function testModifierAtPluginResult()
    {
        $tpl = $this->smarty->createTemplate('eval:{counter|truncate:5 start=100000}');
        $this->assertEquals("10...", $this->smarty->fetch($tpl));
    }

    /**
     * test unqouted string as modifier parameter
     */
    public function testModifierUnqoutedString()
    {
        $tpl = $this->smarty->createTemplate('eval:{"hello world"|replace:hello:xxxxx}');
        $this->assertEquals("xxxxx world", $this->smarty->fetch($tpl));
    }

    /**
     * test registered modifier function
     */
    public function testModifierRegisteredFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', 'testmodifier');
        $tpl = $this->smarty->createTemplate('eval:{$foo|testmodifier}');
        $tpl->assign('foo', 2);
        $this->assertEquals("mymodifier function 2", $this->smarty->fetch($tpl));
    }

    /**
     * test registered modifier static class
     */
    public function testModifierRegisteredStaticClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', array('testmodifierclass', 'staticcall'));
        $tpl = $this->smarty->createTemplate('eval:{$foo|testmodifier}');
        $tpl->assign('foo', 1);
        $this->assertEquals("mymodifier static 1", $this->smarty->fetch($tpl));
    }

    /**
     * test registered modifier method call
     */
    public function testModifierRegisteredMethodCall()
    {
        $obj = new testmodifierclass();
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', array($obj, 'method'));
        $tpl = $this->smarty->createTemplate('eval:{$foo|testmodifier}');
        $tpl->assign('foo', 3);
        $this->assertEquals("mymodifier method 3", $this->smarty->fetch($tpl));
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage unknown modifier "unknown"
     * test unknown modifier error
     */
    public function testUnknownModifier()
    {
        $this->smarty->fetch('eval:{"hello world"|unknown}');
    }

    /**
     * test default modifier
     */
    public function testDefaultModifier()
    {
        $this->smarty->default_modifiers = array('escape');
        $tpl = $this->smarty->createTemplate('eval:{$foo}{$foo nofilter}');
        $tpl->assign('foo', '<bar>');
        $this->assertEquals('&lt;bar&gt;<bar>', $this->smarty->fetch($tpl));
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
