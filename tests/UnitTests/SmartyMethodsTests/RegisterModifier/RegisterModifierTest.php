<?php
/**
 * Smarty PHPunit tests register->modifier / unregister->modifier methods
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for register->modifier / unregister->modifier methods tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class RegisterModifierTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test register->modifier method for function
     */
    public function testRegisterModifier()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', 'mymodifier');
        $this->assertEquals('mymodifier', $this->smarty->registered_plugins[Smarty::PLUGIN_MODIFIER]['testmodifier'][0]);
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals('foo function blar bar', $this->smarty->fetch('eval:{$foo|testmodifier:blar:$bar}'));
    }

    /**
     * test register->modifier method for classes
     */
    public function testRegisterModifierClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', array('mymodifierclass', 'static_method'));
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals('foo static blar bar', $this->smarty->fetch('eval:{$foo|testmodifier:blar:$bar}'));
    }

    /**
     * test register->modifier method for objects
     */
    public function testRegisterModifierObject()
    {
        $obj = new mymodifierclass;
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', array($obj, 'object_method'));
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals('foo object blar bar', $this->smarty->fetch('eval:{$foo|testmodifier:blar:$bar}'));
    }

    /**
     * test unregister->modifier method
     */
    public function testUnregisterModifier()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', 'mymodifier');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_MODIFIER]['testmodifier']));
    }

    /**
     * test unregister->modifier method not registered
     */
    public function testUnregisterModifierNotRegistered()
    {
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_MODIFIER]['testmodifier']));
    }

    /**
     * test unregister->modifier method other registered
     */
    public function testUnregisterModifierOtherRegistered()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testmodifier', 'mymodifier');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier');
        $this->assertTrue(isset($this->smarty->registered_plugins[Smarty::PLUGIN_BLOCK]['testmodifier']));
    }
}

function mymodifier($a, $b, $c)
{
    return "$a function $b $c";
}

class mymodifierclass
{
    static function static_method($a, $b, $c)
    {
        return "$a static $b $c";
    }

    public function object_method($a, $b, $c)
    {
        return "$a object $b $c";
    }
}
