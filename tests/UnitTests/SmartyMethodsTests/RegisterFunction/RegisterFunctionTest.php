<?php
/**
 * Smarty PHPunit tests register/unregister function plugins
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for register/unregister function plugins methods tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 *
 */
class RegisterFunctionTest extends PHPUnit_Smarty
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
     * test registerPlugin method for function
     */
    public function testRegisterFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction');
        $this->assertEquals('myfunction',
                            $this->smarty->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ][ 0 ]);
        $this->assertEquals('hello world 1', $this->smarty->fetch('eval:{testfunction value=1}'));
    }

    /**
     * test registerPlugin method for function  class
     */
    public function testRegisterFunctionClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', array('myfunctionclass', 'execute'));
        $this->assertEquals('hello world 2', $this->smarty->fetch('eval:{testfunction value=2}'));
    }

    /**
     * test registerPlugin method for function object
     */
    public function testRegisterFunctionObject()
    {
        $myfunction_object = new myfunctionclass;
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', array($myfunction_object, 'execute'));
        $this->assertEquals('hello world 3', $this->smarty->fetch('eval:{testfunction value=3}'));
    }

    /**
     * test registerPlugin method for function cached
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testRegisterFunctionCaching1()
    {
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setForceCompile(true);
        $this->smarty->assign('x', 0);
        $this->smarty->assign('y', 10);
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction');
        $this->assertEquals('hello world 0 10', $this->smarty->fetch('test_register_function.tpl'));
    }

    /**
     * test registerPlugin method for function cached
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterFunctionCaching2()
    {
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('x', 1);
        $this->smarty->assign('y', 20);
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction');
        $this->assertEquals('hello world 0 10', $this->smarty->fetch('test_register_function.tpl'));
    }

    /**
     * test registerPlugin method for function not cached
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterFunctionCaching3()
    {
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setForceCompile(true);
        $this->smarty->assign('x', 2);
        $this->smarty->assign('y', 30);
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction', false);
        $this->assertEquals('hello world 2 30', $this->smarty->fetch('test_register_function.tpl'));
    }

    /**
     * test registerPlugin method for function not cached
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterFunctionCaching4()
    {
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('x', 3);
        $this->smarty->assign('y', 40);
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction', false);
        $this->assertEquals('hello world 3 30', $this->smarty->fetch('test_register_function.tpl'));
    }

    /**
     * test unregisterPlugin method for function
     */
    public function testUnregisterFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ]));
    }

    /**
     * test test unregisterPlugin method for function not registered
     */
    public function testUnregisterFunctionNotRegistered()
    {
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ]));
    }

    /**
     * test test unregisterPlugin method for function other registered
     */
    public function testUnregisterFunctionOtherRegistered()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testfunction', 'myfunction');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertTrue(isset($this->smarty->registered_plugins[ Smarty::PLUGIN_BLOCK ][ 'testfunction' ]));
    }
}

function myfunction($params, $smarty)
{
    return "hello world $params[value]";
}

class myfunctionclass
{
    static function execute($params, $smarty)
    {
        return "hello world $params[value]";
    }
}
