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
     * test block plugin repeat function
     *
     */
    public function testBlockPluginRepeat()
    {
        $this->assertEquals('12345', $this->smarty->fetch('repeat.tpl'));
    }

    /**
     * test block plugin repeat function with modifier
     *
     */
    public function testBlockPluginRepeatModidier1()
    {
        $this->assertEquals('11111', $this->smarty->fetch('repeat_modifier.tpl'));
    }

    /**
     * test block plugin repeat function with modifier list
     *
     */
    public function testBlockPluginRepeatModidier2()
    {
        $this->assertEquals('11111', $this->smarty->fetch('repeat_modifier_2.tpl'));
    }
    /**
     * test block plugin with no output
     *
     */
    public function testBlockPluginNoOutput()
    {
        $this->assertEquals('default', $this->smarty->fetch('nooutput.tpl'));
    }
}

function myblockplugintest($params, $content, &$smarty_tpl, &$repeat)
{
    if (!$repeat) {
        $output = str_replace('hello world', 'block test', $content);

        return $output;
    }
}
