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
        $this->setUpSmarty(__DIR__);
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $this->smarty->setForceCompile(true);
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
        $tpl = $this->smarty->createTemplate("eval:{textformat}hello world{/textformat}");
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test block plugin tag with assign attribute
     *
     */
    public function testBlockPluginAssign()
    {
        $tpl = $this->smarty->createTemplate("eval:{textformat assign=foo}hello world{/textformat}{\$foo}");
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test block plugin tag in template file
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginFromTemplateFile()
    {
        $tpl = $this->smarty->createTemplate('blockplugintest.tpl');
        $this->assertEquals("abc", $this->smarty->fetch($tpl));
    }

    /**
     * test block plugin tag in compiled template file
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginFromCompiledTemplateFile()
    {
        $this->smarty->setForceCompile(false);
        $tpl = $this->smarty->createTemplate('blockplugintest.tpl');
        $this->assertEquals("abc", $this->smarty->fetch($tpl));
    }

    /**
     * test block plugin tag in template file
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginFromTemplateFileCache()
    {
        $this->smarty->setForceCompile(false);
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 10;
        $tpl = $this->smarty->createTemplate('blockplugintest.tpl');
        $this->assertEquals("abc", $this->smarty->fetch($tpl));
    }

    /**
     * test block plugin function definition in script
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testBlockPluginRegisteredFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'blockplugintest', 'myblockplugintest');
        $tpl = $this->smarty->createTemplate('eval:{blockplugintest}hello world{/blockplugintest}');
        $this->assertEquals('block test', $this->smarty->fetch($tpl));
    }

    /**
     * test block plugin repeat function
     *
     */
    public function testBlockPluginRepeat()
    {
        $this->smarty->addPluginsDir(dirname(__FILE__) . "/PHPunitplugins/");
        $this->assertEquals('12345', $this->smarty->fetch('eval:{testblock}{/testblock}'));
    }

    /**
     *
     *
     */
    public function testBlockPluginRepeatModidier1()
    {
        $this->smarty->addPluginsDir(dirname(__FILE__) . "/PHPunitplugins/");
        $this->assertEquals('11111', $this->smarty->fetch('eval:{testblock}{/testblock|strlen}'));
    }

    /**
     *
     *
     */
    public function testBlockPluginRepeatModidier2()
    {
        $this->smarty->addPluginsDir(dirname(__FILE__) . "/PHPunitplugins/");
        $this->assertEquals('11111', $this->smarty->fetch('eval:{testblock}{/testblock|strlen|default:""}'));
    }
}

function myblockplugintest($params, $content, &$smarty_tpl, &$repeat)
{
    if (!$repeat) {
        $output = str_replace('hello world', 'block test', $content);

        return $output;
    }
}
