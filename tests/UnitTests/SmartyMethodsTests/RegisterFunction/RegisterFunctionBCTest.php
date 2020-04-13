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
class RegisterFunctionBCTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;

    public $loadSmarty = false;

    public function setUp()
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
        $this->smartyBC->register_function('testfunction', 'myfunctionBC');
        $this->assertEquals('myfunctionBC',
                            $this->smartyBC->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ][ 0 ]);
        $this->assertEquals('hello world 1', $this->smartyBC->fetch('eval:{testfunction value=1}'));
    }

    /**
     * test registerPlugin method for function  class
     */
    public function testRegisterFunctionClass()
    {
        $this->smartyBC->register_function('testfunction', array('myfunctionBCclass', 'execute'));
        $this->assertEquals('hello world 2', $this->smartyBC->fetch('eval:{testfunction value=2}'));
    }

    /**
     * test registerPlugin method for function object
     */
    public function testRegisterFunctionObject()
    {
        $myfunctionBC_object = new myfunctionBCclass;
        $this->smartyBC->register_function('testfunction', array($myfunctionBC_object, 'execute'));
        $this->assertEquals('hello world 3', $this->smartyBC->fetch('eval:{testfunction value=3}'));
    }

    /**
     * test registerPlugin method for function cached
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testRegisterFunctionCaching1()
    {
        $this->smartyBC->caching = 1;
        $this->smartyBC->cache_lifetime = 1000;
        $this->smartyBC->setForceCompile(true);
        $this->smartyBC->assign('x', 0);
        $this->smartyBC->assign('y', 10);
        $this->smartyBC->register_function('testfunction', 'myfunctionBC');
        $this->assertEquals('hello world 0 10', $this->smartyBC->fetch('test_register_function.tpl'));
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
        $this->smartyBC->caching = 1;
        $this->smartyBC->cache_lifetime = 1000;
        $this->smartyBC->assign('x', 1);
        $this->smartyBC->assign('y', 20);
        $this->smartyBC->register_function('testfunction', 'myfunctionBC');
        $this->assertEquals('hello world 0 10', $this->smartyBC->fetch('test_register_function.tpl'));
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
        $this->smartyBC->caching = 1;
        $this->smartyBC->cache_lifetime = 1000;
        $this->smartyBC->setForceCompile(true);
        $this->smartyBC->assign('x', 2);
        $this->smartyBC->assign('y', 30);
        $this->smartyBC->register_function('testfunction', 'myfunctionBC', false);
        $this->assertEquals('hello world 2 30', $this->smartyBC->fetch('test_register_function.tpl'));
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
        $this->smartyBC->caching = 1;
        $this->smartyBC->cache_lifetime = 1000;
        $this->smartyBC->assign('x', 3);
        $this->smartyBC->assign('y', 40);
        $this->smartyBC->register_function('testfunction', 'myfunctionBC', false);
        $this->assertEquals('hello world 3 30', $this->smartyBC->fetch('test_register_function.tpl'));
    }

    /**
     * test unregisterPlugin method for function
     */
    public function testUnregisterFunction()
    {
        $this->smartyBC->register_function('testfunction', 'myfunctionBC');
        $this->smartyBC->unregister_function('testfunction');
        $this->assertFalse(isset($this->smartyBC->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ]));
    }

    /**
     * test test unregisterPlugin method for function not registered
     */
    public function testUnregisterfunctionotRegistered()
    {
        $this->smartyBC->unregister_function('testfunction');
        $this->assertFalse(isset($this->smartyBC->registered_plugins[ Smarty::PLUGIN_FUNCTION ][ 'testfunction' ]));
    }

    /**
     * test test unregisterPlugin method for function other registered
     */
    public function testUnregisterFunctionOtherRegistered()
    {
        $this->smartyBC->register_block('testfunction', 'myfunctionBC');
        $this->smartyBC->unregister_function('testfunction');
        $this->assertTrue(isset($this->smartyBC->registered_plugins[ Smarty::PLUGIN_BLOCK ][ 'testfunction' ]));
    }
}

function myfunctionBC($params, $smarty)
{
    return "hello world $params[value]";
}

class myfunctionBCclass
{
    static function execute($params, $smarty)
    {
        return "hello world $params[value]";
    }
}
