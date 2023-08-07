<?php
/**
 * Smarty PHPunit tests register->resource
 *

 * @author  Uwe Tews
 */

use Smarty\Resource\CustomPlugin;

/**
 * class for register->resource tests
 *
 * 
 * 
 *
 */
class RegisteredResourceTest extends PHPUnit_Smarty
{

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);

        $this->smarty->registerResource("rr", new RegisteredResourceTest_Resource1Plugin());
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
        $this->assertEquals('hello world', $this->smarty->fetch('rr:test'));
    }

    public function testClearCompiledResourcePlugin()
    {
        $this->assertEquals(1, $this->smarty->clearCompiledTemplate('rr:test'));
    }

    /**
     * test resource plugin timesatmp
     */
    public function testResourcePluginTimestamp()
    {
        $tpl = $this->smarty->createTemplate('rr:test');
        $this->assertTrue(is_integer($tpl->getSource()->getTimeStamp()));
        $this->assertEquals(10, strlen($tpl->getSource()->getTimeStamp()));
    }

    /**
     * test compile_id change
     */
    public function testResourceCompileIdChange()
    {
        $this->smarty->registerResource('myresource', new RegisteredResourceTest_Resource2Plugin());
        $this->smarty->setCompileId('a');
        $this->assertEquals('this is template 1', $this->smarty->fetch('myresource:some'));
        $this->assertEquals('this is template 1', $this->smarty->fetch('myresource:some'));
        $this->smarty->setCompileId('b');
        $this->assertEquals('this is template 2', $this->smarty->fetch('myresource:some'));
        $this->assertEquals('this is template 2', $this->smarty->fetch('myresource:some'));
    }
    /**
     * test {$smarty.template}
     *
     */
    public function testSmartyTemplate() {
        $this->smarty->registerResource('mytpl', new RegisteredResourceTest_Resource3Plugin());
        $this->assertEquals('template = mytpl:foo', $this->smarty->fetch('mytpl:foo'));
    }
    /**
     * test {$smarty.current_dir}
     *
     */
    public function testSmartyCurrentDir() {
        $this->smarty->registerResource('mytpl', new RegisteredResourceTest_Resource4Plugin());
        $this->assertEquals('current_dir = .', $this->smarty->fetch('mytpl:bar'));
    }
}

class RegisteredResourceTest_Resource1Plugin extends CustomPlugin {

    protected function fetch($name, &$source, &$mtime) {
        $source = '{$x="hello world"}{$x}';
        $mtime = 1000000000;
    }

}

class RegisteredResourceTest_Resource2Plugin extends CustomPlugin {

    protected function fetch($name, &$source, &$mtime) {

        // we update a counter, so that we return a new source for every call
        static $counter = 0;
        $counter ++;

        // construct a new source
        $source = "this is template $counter";

        $mtime = 1000000000;
    }

    protected function fetchTimestamp($name)
    {
        return 1000000000;
    }

}

class RegisteredResourceTest_Resource3Plugin extends CustomPlugin {

    protected function fetch($name, &$source, &$mtime) {
        $source = 'template = {$smarty.template}';
        $mtime = 1000000000;
    }

}

class RegisteredResourceTest_Resource4Plugin extends CustomPlugin {

    protected function fetch($name, &$source, &$mtime) {
        $source = 'current_dir = {$smarty.current_dir}';
        $mtime = 1000000000;
    }

}
