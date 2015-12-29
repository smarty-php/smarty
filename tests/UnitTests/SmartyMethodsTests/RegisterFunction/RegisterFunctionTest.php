<?php
/**
 * Smarty PHPunit tests register/unregister function plugins
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
    public $loadSmartyBC = true;

    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
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
        $this->assertEquals('hello world 1', $this->smarty->fetch('string:{testfunction value=1}'));
    }

    /**
     * test wrapper for register_function method
     */
    public function testRegisterFunctionWrapper()
    {
        $this->smarty->register_function('testfunction', 'myfunction');
        $this->assertEquals('myfunction',
                            $this->smarty->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ][ 0 ]);
        $this->assertEquals('hello world 12', $this->smarty->fetch('string:{testfunction value=12}'));
    }

    /**
     * test SmartyBC for register_function method
     */

    public function testRegisterFunctionSmartyBC()
    {
        $this->smartyBC->register_function('testfunction', 'myfunction');
        $this->assertEquals('myfunction',
                            $this->smartyBC->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ][ 0 ]);
        $this->assertEquals('hello world 13', $this->smartyBC->fetch('string:{testfunction value=13}'));
    }

    /**
     * test registerPlugin method for function plugin by class
     */
    public function testRegisterFunctionClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', array('myfunctionclass', 'execute'));
        $this->assertEquals('hello world 2', $this->smarty->fetch('string:{testfunction value=2}'));
    }

    /**
     * test register_function wrapper method for function plugin by class
     */
    public function testRegisterFunctionClassWrapper()
    {
        $this->smarty->register_function('testfunction', array('myfunctionclass', 'execute'));
        $this->assertEquals('hello world 12', $this->smarty->fetch('string:{testfunction value=12}'));
    }

    /**
     * test register_function SmartyBC method for function plugin by class
     */
    public function testRegisterFunctionClassSmartyBC()
    {
        $this->smartyBC->register_function('testfunction', array('myfunctionclass', 'execute'));
        $this->assertEquals('hello world 22', $this->smartyBC->fetch('string:{testfunction value=22}'));
    }

    /**
     * test registerPlugin method for function plugin by object
     */
    public function testRegisterFunctionObject()
    {
        $myfunction_object = new myfunctionclass;
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', array($myfunction_object, 'execute'));
        $this->assertEquals('hello world 3', $this->smarty->fetch('string:{testfunction value=3}'));
    }

    /**
     * test register_function wrapper method for function plugin by object
     */
    public function testRegisterFunctionObjectWrapper()
    {
        $myfunction_object = new myfunctionclass;
        $this->smarty->register_function('testfunction', array($myfunction_object, 'execute'));
        $this->assertEquals('hello world 13', $this->smarty->fetch('string:{testfunction value=13}'));
    }

    /**
     * test register_function SmartyBC method for function plugin by object
     */
    public function testRegisterFunctionObjectSmartyBC()
    {
        $myfunction_object = new myfunctionclass;
        $this->smartyBC->register_function('testfunction', array($myfunction_object, 'execute'));
        $this->assertEquals('hello world 23', $this->smartyBC->fetch('string:{testfunction value=23}'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterFunctionCaching1()
    {
        $this->smarty->setCaching(1);;
        $this->smarty->setCacheLifetime(1000);;
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
        $this->smarty->setCaching(1);;
        $this->smarty->setCacheLifetime(1000);;
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
        $this->smarty->setCaching(1);;
        $this->smarty->setCacheLifetime(1000);;
        $this->smarty->setCompileId(1);
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
    public function testRegisterFunctionCaching3BC()
    {
        $this->smartyBC->setCaching(1);;
        $this->smartyBC->setCacheLifetime(1000);;
        $this->smartyBC->setCompileId(2);
        $this->smartyBC->assign('x', 12);
        $this->smartyBC->assign('y', 130);
        $this->smartyBC->register_function('testfunction', 'myfunction', false);
        $this->assertEquals('hello world 12 130', $this->smartyBC->fetch('test_register_function.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterFunctionCaching4()
    {
        $this->smarty->setCaching(1);;
        $this->smarty->setCacheLifetime(1000);;
        $this->smarty->setCompileId(1);
        $this->smarty->assign('x', 3);
        $this->smarty->assign('y', 40);
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction', false);
        $this->assertEquals('hello world 3 30', $this->smarty->fetch('test_register_function.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterFunctionCaching4BC()
    {
        $this->smartyBC->setCaching(1);;
        $this->smartyBC->setCacheLifetime(1000);;
        $this->smartyBC->setCompileId(2);
        $this->smartyBC->assign('x', 13);
        $this->smartyBC->assign('y', 140);
        $this->smartyBC->register_function('testfunction', 'myfunction', false);
        $this->assertEquals('hello world 13 130', $this->smartyBC->fetch('test_register_function.tpl'));
    }

    /**
     * test unregister->templateFunction method
     */
    public function testUnregisterFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction', 'myfunction');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ]));
    }

    /**
     * test unregister_function method
     */
    public function testUnregisterFunctionWrapper()
    {
        $this->smarty->register_function('testfunction', 'myfunction');
        $this->smarty->unregister_function('testfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ]));
    }

    public function testUnregisterFunctionSmartyBC()
    {
        $this->smartyBC->register_function('testfunction', 'myfunction');
        $this->smartyBC->unregister_function('testfunction');
        $this->assertFalse(isset($this->smartyBC->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ]));
    }

    /**
     * test unregister->templateFunction method not registered
     */
    public function testUnregisterFunctionNotRegistered()
    {
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ]));
    }

    /**
     * test unregister->templateFunction method other registered
     */
    public function testUnregisterFunctionOtherRegistered()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testfunction', 'myfunction');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_FUNCTION, 'testfunction');
        $this->assertTrue(isset($this->smarty->registered_plugins[ Smarty::PLUGIN_BLOCK ][ 'testfunction' ]));
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
