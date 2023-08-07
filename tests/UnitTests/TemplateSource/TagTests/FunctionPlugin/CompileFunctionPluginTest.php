<?php
/**
 * Smarty PHPunit tests compilation of function plugins
 *

 * @author  Uwe Tews
 */

/**
 * class for function plugin tests
 *
 *
 * 
 * 
 */
class CompileFunctionPluginTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }


    /**
     * test function plugin tag in compiled template file
     */
    public function testFunctionPluginFromCompiledTemplateFile()
    {
        $this->smarty->setForceCompile(false);
        $tpl = $this->smarty->createTemplate('functionplugintest.tpl', $this->smarty);
        $this->assertEquals("10", $this->smarty->fetch($tpl));
    }

    /**
     * test function plugin function definition in script
     */
    public function testFunctionPluginRegisteredFunction()
    {
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_FUNCTION, 'plugintest', 'myplugintest');
        $tpl = $this->smarty->createTemplate('string:{plugintest foo=bar}', $this->smarty);
        $this->assertEquals("plugin test called bar", $this->smarty->fetch($tpl));
    }

    /**
     * test muiltiline tags
     */
    public function testMultiLineTags()
    {
        $this->assertEquals("10", $this->smarty->fetch("string:{counter\n\tstart=10}"));
    }
}

function myplugintest($params, $smarty)
{
    return "plugin test called $params[foo]";
}
