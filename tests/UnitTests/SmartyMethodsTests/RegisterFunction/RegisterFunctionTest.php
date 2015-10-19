<?php
/**
 * Smarty PHPunit tests register->templateFunction / unregister->templateFunction methods
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for register->templateFunction / unregister->templateFunction methods tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 *
 */
class RegisterFunctionTest extends PHPUnit_Smarty
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
     * test register->templateFunction method for function
     */
    public function testRegisterFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction');
        $this->assertEquals('myfunction', $this->smarty->registered_plugins[Smarty::PLUGIN_FUNCTION]['testfunction'][0]);
        $this->assertEquals('hello world 1', $this->smarty->fetch('eval:{testfunction value=1}'));
    }

    /**
     * test wrapper rfor egister_function method for function
     */
    public function testRegisterFunctionWrapper()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction');
        $this->assertEquals('myfunction', $this->smarty->registered_plugins[Smarty::PLUGIN_FUNCTION]['testfunction'][0]);
        $this->assertEquals('hello world 1', $this->smarty->fetch('eval:{testfunction value=1}'));
    }

    /**
     * test register->templateFunction method for class
     */
    public function testRegisterFunctionClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', array('myfunctionclass', 'execute'));
        $this->assertEquals('hello world 2', $this->smarty->fetch('eval:{testfunction value=2}'));
    }

    /**
     * test register->templateFunction method for object
     */
    public function testRegisterFunctionObject()
    {
        $myfunction_object = new myfunctionclass;
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', array($myfunction_object, 'execute'));
        $this->assertEquals('hello world 3', $this->smarty->fetch('eval:{testfunction value=3}'));
    }

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
     * test unregister->templateFunction method
     */
    public function testUnregisterFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_FUNCTION]['testfunction']));
    }

    /**
     * test unregister->templateFunction method not registered
     */
    public function testUnregisterFunctionNotRegistered()
    {
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_FUNCTION]['testfunction']));
    }

    /**
     * test unregister->templateFunction method other registered
     */
    public function testUnregisterFunctionOtherRegistered()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testfunction', 'myfunction');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertTrue(isset($this->smarty->registered_plugins[Smarty::PLUGIN_BLOCK]['testfunction']));
    }
}

function myfunction($params, &$smarty)
{
    return "hello world $params[value]";
}

class myfunctionclass
{
    static function execute($params, &$smarty)
    {
        return "hello world $params[value]";
    }
}
