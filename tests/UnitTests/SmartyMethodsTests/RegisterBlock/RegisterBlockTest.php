<?php
/**
 * Smarty PHPunit tests register->block / unregister->block methods
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for register->block / unregister->block methods tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class RegisterBlockTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->disableSecurity();
        $this->smartyBC->disableSecurity();
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test registerPlugin method for block function
     */
    public function testRegisterBlockFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', 'myblock');
        $this->smarty->assign('value', 1);
        $this->assertEquals('function hello world 1 1 function hello world 1 2 function hello world 1 3 ', $this->smarty->fetch('eval:{testblock}hello world {$value}{/testblock}'));
    }

    public function testRegisterBlockFunctionModifier1()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', 'myblock');
        $this->smarty->assign('value', 1);
        $this->assertEquals(strtoupper('function hello world 1 1 function hello world 1 2 function hello world 1 3 '), $this->smarty->fetch('eval:{testblock}hello world {$value}{/testblock|strtoupper}'));
    }

    public function testRegisterBlockFunctionModifier2()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', 'myblock');
        $this->smarty->assign('value', 1);
        $this->assertEquals(strtoupper('function hello world 1 1 function hello world 1 2 function hello world 1 3 '), $this->smarty->fetch('eval:{testblock}hello world {$value}{/testblock|default:""|strtoupper}'));
    }

    public function testRegisterBlockFunctionWrapper()
    {
        $this->smartyBC->register_block('testblock', 'myblock');
        $this->smartyBC->assign('value', 1);
        $this->assertEquals('function hello world 1 1 function hello world 1 2 function hello world 1 3 ', $this->smartyBC->fetch('eval:{testblock}hello world {$value}{/testblock}'));
    }

    /**
     * test registerPlugin method for block class
     */
    public function testRegisterBlockClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', array('myblockclass', 'static_method'));
        $this->smarty->assign('value', 2);
        $this->assertEquals('static hello world 2 1 static hello world 2 2 static hello world 2 3 ', $this->smarty->fetch('eval:{testblock}hello world {$value}{/testblock}'));
    }

    public function testRegisterBlockClassWrapper()
    {
        $this->smartyBC->register_block('testblock', array('myblockclass', 'static_method'));
        $this->smartyBC->assign('value', 2);
        $this->assertEquals('static hello world 2 1 static hello world 2 2 static hello world 2 3 ', $this->smartyBC->fetch('eval:{testblock}hello world {$value}{/testblock}'));
    }

    /**
     * test registerPlugin method for block object
     */
    public function testRegisterBlockObject()
    {
        $myblock_object = new myblockclass;
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', array($myblock_object, 'object_method'));
        $this->smarty->assign('value', 3);
        $this->assertEquals('object hello world 3 1 object hello world 3 2 object hello world 3 3 ', $this->smarty->fetch('eval:{testblock}hello world {$value}{/testblock}'));
    }

    public function testRegisterBlockObjectWrapper()
    {
        $myblock_object = new myblockclass;
        $this->smartyBC->register_block('testblock', array($myblock_object, 'object_method'));
        $this->smartyBC->assign('value', 3);
        $this->assertEquals('object hello world 3 1 object hello world 3 2 object hello world 3 3 ', $this->smartyBC->fetch('eval:{testblock}hello world {$value}{/testblock}'));
    }

    /**
     * test registerPlugin method for block with caching
     */
    public function testRegisterBlockCaching1()
    {
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setForceCompile(true);
        $this->smarty->assign('x', 1);
        $this->smarty->assign('y', 10);
        $this->smarty->assign('z', 100);
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', 'myblockcache');
        $this->assertEquals('1 10 100', $this->smarty->fetch('test_register_block.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterBlockCaching2()
    {
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('x', 2);
        $this->smarty->assign('y', 20);
        $this->smarty->assign('z', 200);
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', 'myblockcache');
        $this->assertEquals('1 10 100', $this->smarty->fetch('test_register_block.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterBlockCaching3()
    {
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setForceCompile(true);
        $this->smarty->assign('x', 3);
        $this->smarty->assign('y', 30);
        $this->smarty->assign('z', 300);
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', 'myblockcache', false);
        $this->assertEquals('3 30 300', $this->smarty->fetch('test_register_block.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterBlockCaching4()
    {
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('x', 4);
        $this->smarty->assign('y', 40);
        $this->smarty->assign('z', 400);
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', 'myblockcache', false);
        $this->assertEquals('3 40 300', $this->smarty->fetch('test_register_block.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterBlockCaching1Wrapper()
    {
        $this->smartyBC->caching = 1;
        $this->smartyBC->cache_lifetime = 1000;
        $this->smartyBC->setForceCompile(true);
        $this->smartyBC->assign('x', 1);
        $this->smartyBC->assign('y', 10);
        $this->smartyBC->assign('z', 100);
        $this->smartyBC->register_block('testblock', 'myblockcache');
        $this->assertEquals('1 10 100', $this->smartyBC->fetch('test_register_block.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterBlockCaching2Wrapper()
    {
        $this->smartyBC->caching = 1;
        $this->smartyBC->cache_lifetime = 1000;
        $this->smartyBC->assign('x', 2);
        $this->smartyBC->assign('y', 20);
        $this->smartyBC->assign('z', 200);
        $this->smartyBC->register_block('testblock', 'myblockcache');
        $this->assertEquals('1 10 100', $this->smartyBC->fetch('test_register_block.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterBlockCaching3Wrapper()
    {
        $this->smartyBC->caching = 1;
        $this->smartyBC->cache_lifetime = 1000;
        $this->smartyBC->setForceCompile(true);
        $this->smartyBC->assign('x', 3);
        $this->smartyBC->assign('y', 30);
        $this->smartyBC->assign('z', 300);
        $this->smartyBC->register_block('testblock', 'myblockcache', false);
        $this->assertEquals('3 30 300', $this->smartyBC->fetch('test_register_block.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testRegisterBlockCaching4Wrapper()
    {
        $this->smartyBC->caching = 1;
        $this->smartyBC->cache_lifetime = 1000;
        $this->smartyBC->assign('x', 4);
        $this->smartyBC->assign('y', 40);
        $this->smartyBC->assign('z', 400);
        $this->smartyBC->register_block('testblock', 'myblockcache', false);
        $this->assertEquals('3 40 300', $this->smartyBC->fetch('test_register_block.tpl'));
    }

    /**
     * test unregister->block method
     */
    public function testUnregisterBlock()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testblock', 'myblock');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_BLOCK, 'testblock');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_BLOCK]['testblock']));
    }

    public function testUnregisterBlockWrapper()
    {
        $this->smartyBC->register_block('testblock', 'myblock');
        $this->smartyBC->unregister_block('testblock');
        $this->assertFalse(isset($this->smartyBC->registered_plugins[Smarty::PLUGIN_BLOCK]['testblock']));
    }

    /**
     * test unregister->block method not registered
     */
    public function testUnregisterBlockNotRegistered()
    {
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_BLOCK, 'testblock');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_BLOCK]['testblock']));
    }
}

function myblock($params, $content, &$smarty_tpl, &$repeat)
{
    static $loop = 0;

    if ($content == null) {
        $loop = 0;

        return;
    }
    $loop ++;
    if ($loop < 3) {
        $repeat = true;
    }

    return "function $content $loop ";
}

function myblockcache($params, $content, &$smarty_tpl, &$repeat)
{
    return $content;
}

class myblockclass
{
    static function static_method($params, $content, &$smarty_tpl, &$repeat)
    {
        static $loop = 0;

        if ($content == null) {
            $loop = 0;

            return;
        }
        $loop ++;
        if ($loop < 3) {
            $repeat = true;
        }

        return "static $content $loop ";
    }

    public function object_method($params, $content, &$smarty_tpl, &$repeat)
    {
        static $loop = 0;

        if ($content == null) {
            $loop = 0;

            return;
        }
        $loop ++;
        if ($loop < 3) {
            $repeat = true;
        }

        return "object $content $loop ";
    }
}
