<?php
/**
 * Smarty PHPunit tests compiler plugin
 *

 * @author  Uwe Tews
 */

/**
 * class for compiler plugin tests
 */
class CompilerPluginTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test compiler plugin
     */
    public function testCompilerPlugin()
    {
        $this->smarty->addPluginsDir(__DIR__ . "/PHPunitplugins/");
        $this->assertEquals('test output', $this->smarty->fetch('eval:{test data="test output"}{/test}'));
    }
}
