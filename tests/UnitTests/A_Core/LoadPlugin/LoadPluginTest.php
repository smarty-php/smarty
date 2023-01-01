<?php
/**
 * Smarty PHPunit basic core function tests
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class core function tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class LoadPluginTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    /**
     * loadPlugin test unkown plugin
     */
    public function testLoadPluginErrorReturn()
    {
        $this->assertFalse($this->smarty->loadPlugin('\\Smarty\\Not\\Known'));
    }

    /**
     * loadPlugin test \Smarty\Debug exists
     */
    public function testLoadPluginSmartyInternalDebug()
    {
        $this->assertTrue($this->smarty->loadPlugin(\Smarty\Debug::class) == true);
    }

    /**
     * loadPlugin test \Smarty\Template exists
     */
    public function testLoadPluginSmartyTemplateClass()
    {
        $this->assertTrue($this->smarty->loadPlugin(\Smarty\Template) == true);
    }

    /**
     * loadPlugin test loaging from plugins_dir
     */
    public function testLoadPluginSmartyPluginCounter()
    {
        $this->assertTrue($this->smarty->loadPlugin('smarty_function_counter') == true);
    }
}
