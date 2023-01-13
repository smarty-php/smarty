<?php
/**
 * Smarty PHPunit tests resource plugins
 *

 * @author  Uwe Tews
 */

/**
 * class for resource plugins tests
 *
 *
 * 
 *
 */
class ResourcePluginTest extends PHPUnit_Smarty
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
     * test resource plugin rendering
     */
    public function testResourcePlugin()
    {
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $this->assertEquals('hello world', $this->smarty->fetch('db:test'));
    }

    /**
     * test resource plugin rendering
     */
    public function testResourcePluginObject()
    {
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $this->assertEquals('hello world', $this->smarty->fetch('db2:test'));
    }

    /**
     * test resource plugin rendering of a registered object
     */
    public function testResourcePluginRegisteredInstance()
    {
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $this->smarty->registerResource('db2a', new Smarty_Resource_Db2('db2a'));
        $this->assertEquals('hello world', $this->smarty->fetch('db2a:test'));
    }

    /**
     * test resource plugin non-existent compiled cache of a recompiling resource
     */
    public function testResourcePluginRecompiledCompiledFilepath()
    {
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $tpl = $this->smarty->createTemplate('db2:test.tpl');
        $expected = realpath('./templates_c/' . sha1('db2:test.tpl') . '.db2.test.tpl.php');
        $this->assertFalse(!!$expected);
        $this->assertNull($tpl->getCompiled()->filepath);
    }

    /**
     * test resource plugin timestamp
     */
    public function testResourcePluginTimestamp()
    {
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $tpl = $this->smarty->createTemplate('db:test');
        $this->assertTrue(is_integer($tpl->getSource()->getTimeStamp()));
        $this->assertEquals(10, strlen($tpl->getSource()->getTimeStamp()));
    }
}

