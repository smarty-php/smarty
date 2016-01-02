<?php
/**
 * Smarty PHPunit tests compilation of block plugins
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for block plugin tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileBlockPluginTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $this->smarty->disableSecurity();
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test block plugin tag
     *
     */
    public function testBlockPluginNoAssign()
    {
         $this->assertEquals("hello world", $this->smarty->fetch('no_assign.tpl'));
    }

    /**
     * test block plugin tag with assign attribute
     *
     */
    public function testBlockPluginAssign()
    {
        $this->assertEquals("hello world", $this->smarty->fetch('assign.tpl'));
    }

    /**
     * test block plugin function definition in script
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginRegisteredFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'blockplugintest', 'myblockplugintest');
        $this->assertEquals('block test', $this->smarty->fetch('registered.tpl'));
    }

    /**
     * test block plugin static method
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginRegisteredStatic()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'blockpluginstatic', array('myblockclass1', 'staticfunc'));
        $this->assertEquals('static block test', $this->smarty->fetch('registered_static.tpl'));
    }

    /**
     * test block plugin object method
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginRegisteredMethod()
    {
        $object = new myblockclass1();
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'blockpluginmethod', array($object, 'methodfunc'));
        $this->assertEquals('method block test', $this->smarty->fetch('registered_method.tpl'));
    }
    /**
     * test block plugin registered object
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginRegisteredObject()
    {
        $object = new myblockclass1();
        $this->smarty->registerObject('myobject', $object, array(), true, array('objectfunc'));
        $this->assertEquals('method block test', $this->smarty->fetch('registered_object.tpl'));
    }

    /**
     * test block plugin repeat function
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginRepeat()
    {
        $this->assertEquals('12345', $this->smarty->fetch('repeat.tpl'));
    }

    /**
     * test block plugin repeat function with modifier
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginRepeatModidier1()
    {
        $this->assertEquals('11111', $this->smarty->fetch('repeat_modifier.tpl'));
    }

    /**
     * test block plugin repeat function with modifier list
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginRepeatModidier2()
    {
        $this->assertEquals('11111', $this->smarty->fetch('repeat_modifier_2.tpl'));
    }
    /**
     * test block plugin with no output
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginNoOutput()
    {
        $this->assertEquals('default', $this->smarty->fetch('nooutput.tpl'));
    }
    /**
     * test nested block plugin
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginNested()
    {
        $this->assertEquals('hello world12345', $this->smarty->fetch('nested.tpl'));
    }
    /**
     * test default block plugin
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginDefault1()
    {
        $this->smarty->registerDefaultPluginHandler('my_block_plugin_handler');
        $this->assertEquals('scriptblock hello world', $this->smarty->fetch('default1.tpl'));
    }
    /**
     * test default block plugin
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginDefault2()
    {
        $this->smarty->registerDefaultPluginHandler('my_block_plugin_handler');
        $this->assertEquals('defaultblock hello world', $this->smarty->fetch('default2.tpl'));
    }
}

function myblockplugintest($params, $content, &$smarty_tpl, &$repeat)
{
    if (!$repeat) {
        $output = str_replace('hello world', 'block test', $content);

        return $output;
    }
}

class myblockclass1
{
    static function staticfunc($params, $content, &$smarty_tpl, &$repeat)
    {
        if (!$repeat) {
            $output = str_replace('hello world', 'static block test', $content);
            return $output;
        }
    }
    public function methodfunc($params, $content, &$smarty_tpl, &$repeat)
    {
        if (!$repeat) {
            $output = str_replace('hello world', 'method block test', $content);
            return $output;
        }
    }
    public function objectfunc($params, $content, &$smarty_tpl, &$repeat)
    {
        if (!$repeat) {
            $output = str_replace('hello world', 'object block test', $content);
            return $output;
        }
    }
}
function my_block_plugin_handler($tag, $type, $template, &$callback, &$script, &$cachable)
{
    switch ($type) {
        case Smarty::PLUGIN_BLOCK:
            switch ($tag) {
                case 'scriptblock':
                    $script = './scripts/script_block_tag.php';
                    $callback = 'default_script_block_tag';

                    return true;
                default:
                    $script = './scripts/other_block_tag.php';
                    $callback = 'default_block_tag';
                    return true;
            }
         default:
            return false;
    }
}

