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

    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));

        $this->smarty->registerResource("rr", array("rr_get_template",
                                                    "rr_get_timestamp",
                                                    "rr_get_secure",
                                                    "rr_get_trusted"));
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
        $this->smarty->registerResource('myresource', array('getSource', 'getTimestamp', 'getSecure', 'getTrusted'));
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
        $this->smarty->registerResource('mytpl', array('getTemplate', 'getTimestamp', 'getSecure', 'getTrusted'));
        $this->assertEquals('template = mytpl:foo', $this->smarty->fetch('mytpl:foo'));
    }
    /**
     * test {$smarty.current_dir}
     *
     */
    public function testSmartyCurrentDir() {
        $this->smarty->registerResource('mytpl', array('getCurrentDir', 'getTimestamp', 'getSecure', 'getTrusted'));
        $this->assertEquals('current_dir = .', $this->smarty->fetch('mytpl:bar'));
    }
}


/**
 * resource functions
 */
function rr_get_template($tpl_name, &$tpl_source, $smarty_obj)
{
    // populating $tpl_source
    $tpl_source = '{$x="hello world"}{$x}';

    return true;
}

function rr_get_timestamp($tpl_name, &$tpl_timestamp, $smarty_obj)
{
    // $tpl_timestamp.
    $tpl_timestamp = (int) floor(time() / 100) * 100;

    return true;
}

function rr_get_secure($tpl_name, $smarty_obj)
{
    // assume all templates are secure
    return true;
}

function rr_get_trusted($tpl_name, $smarty_obj)
{
    // not used for templates
}

// resource functions for compile_id change test

function getSecure($name, $smarty)
{
    return true;
}

function getTrusted($name, $smarty)
{
}

function getSource($name, &$source, $smarty)
{
    // we update a counter, so that we return a new source for every call
    static $counter = 0;
    $counter ++;

    // construct a new source
    $source = "this is template $counter";

    return true;
}
function getTemplate($name, &$source, $smarty)
{
    // construct a new source
    $source = 'template = {$smarty.template}';

    return true;
}
function getCurrentDir($name, &$source, $smarty)
{
    // construct a new source
    $source = 'current_dir = {$smarty.current_dir}';

    return true;
}

function getTimestamp($name, &$timestamp, $smarty)
{
    // always pretend the template is brand new
    $timestamp = (int) floor(time() / 100) * 100;

    return true;
}
