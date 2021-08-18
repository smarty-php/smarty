<?php
/**
 * Smarty PHPunit tests register->resource
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for register->resource tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class RegisteredResourceTest extends PHPUnit_Smarty
{

    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));

        $this->smarty->registerResource("rr", new RegisteredResourceTest_Resource1());
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
        $this->assertTrue(is_integer($tpl->source->getTimeStamp()));
        $this->assertEquals(10, strlen($tpl->source->getTimeStamp()));
    }

    /**
     * test compile_id change
     */
    public function testResourceCompileIdChange()
    {
        $this->smarty->registerResource('myresource', new RegisteredResourceTest_Resource2());
        $this->smarty->compile_id = 'a';
        $this->assertEquals('this is template 1', $this->smarty->fetch('myresource:some'));
        $this->assertEquals('this is template 1', $this->smarty->fetch('myresource:some'));
        $this->smarty->compile_id = 'b';
        $this->assertEquals('this is template 2', $this->smarty->fetch('myresource:some'));
        $this->assertEquals('this is template 2', $this->smarty->fetch('myresource:some'));
    }
    /**
     * test {$smarty.template}
     *
     */
    public function testSmartyTemplate() {
        $this->smarty->registerResource('mytpl', new RegisteredResourceTest_Resource3());
        $this->assertEquals('template = mytpl:foo', $this->smarty->fetch('mytpl:foo'));
    }
    /**
     * test {$smarty.current_dir}
     *
     */
    public function testSmartyCurrentDir() {
        $this->smarty->registerResource('mytpl', new RegisteredResourceTest_Resource4());
        $this->assertEquals('current_dir = .', $this->smarty->fetch('mytpl:bar'));
    }
}

class RegisteredResourceTest_Resource1 extends Smarty_Resource_Custom {

    protected function fetch($name, &$source, &$mtime) {
        $source = '{$x="hello world"}{$x}';
        $mtime = 1000000000;
    }

}

class RegisteredResourceTest_Resource2 extends Smarty_Resource_Custom {

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

class RegisteredResourceTest_Resource3 extends Smarty_Resource_Custom {

    protected function fetch($name, &$source, &$mtime) {
        $source = 'template = {$smarty.template}';
        $mtime = 1000000000;
    }

}

class RegisteredResourceTest_Resource4 extends Smarty_Resource_Custom {

    protected function fetch($name, &$source, &$mtime) {
        $source = 'current_dir = {$smarty.current_dir}';
        $mtime = 1000000000;
    }

}
