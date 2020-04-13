<?php
/**
 * Smarty PHPUnit tests default config handler
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for default config handler test
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class DefaultConfigHandlerTest extends PHPUnit_Smarty
{

    /**
     * Sets up the fixture
     * This method is called before a test is executed.
     *
     */
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->setForceCompile(true);
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage Unable to load config 'file:foo.conf'
     *
     * test unknown config file
     */
    public function testUnknownConfigFile()
    {
        $this->smarty->configLoad('foo.conf');
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage Default config handler
     * @expectedExceptionMessage not callable
     *
     * test register unknown default config handler
     *
     */
    public function testRegisterUnknownDefaultConfigHandler()
    {
        $this->smarty->registerDefaultConfigHandler('foo');
    }

    /**
     * test default config handler replacement (config data)
     *
     * @throws \Exception
     * @throws \SmartyException
     */
    public function testDefaultConfigHandlerReplacement()
    {
        $this->smarty->registerDefaultConfigHandler('configHandlerData');
        $this->smarty->configLoad('foo.conf');
        $this->assertEquals("bar", $this->smarty->fetch('foo.tpl'));
    }

    /**
     * test default config handler replacement (other config file)
     *
     * @throws \Exception
     * @throws \SmartyException
     */
    public function testDefaultConfigHandlerReplacementByConfigFile()
    {
        $this->smarty->registerDefaultConfigHandler('configHandlerFile');
        $this->smarty->configLoad('foo.conf');
        $this->assertEquals("123.4", $this->smarty->fetch('number.tpl'));
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage Unable to load config default file 'no.conf' for 'file:fo.conf'
     *
     *
     */
    public function testDefaultConfigHandlerReplacementByConfigFileFail()
    {
        $this->smarty->registerDefaultConfigHandler('configHandlerFile');
        $this->smarty->configLoad('fo.conf');
        $this->assertEquals("123.4", $this->smarty->fetch('number.tpl'));
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage Unable to load config 'file:foo.conf'
     *
     *
     * test default config handler replacement (return false)
     *
     */
    public function testDefaultConfigHandlerReplacementReturningFalse()
    {
        $this->smarty->configLoad('foo.conf');
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage No config default content for 'file:bla.conf'
     *
     *
     * test default config handler replacement (return false)
     *
     */
    public function testDefaultConfigHandlerReplacementReturningFalse1()
    {
        $this->smarty->registerDefaultConfigHandler('configHandlerData');
        $this->smarty->configLoad('bla.conf');
    }

}

/**
 * config handler returning config data
 *
 * @param         $resource_type
 * @param         $resource_name
 * @param         $config_source
 * @param         $config_timestamp
 * @param \Smarty $smarty
 *
 * @return bool
 */
function configHandlerData($resource_type, $resource_name, &$config_source, &$config_timestamp, Smarty $smarty)
{
    if ($resource_name !== 'foo.conf') {
        return false;
    }
    $output = "foo = 'bar'\n";
    $config_source = $output;
    $config_timestamp = time();

    return true;
}

/**
 * config handler returning config file
 *
 * @param         $resource_type
 * @param         $resource_name
 * @param         $config_source
 * @param         $config_timestamp
 * @param \Smarty $smarty
 *
 * @return string
 */
function configHandlerFile($resource_type, $resource_name, &$config_source, &$config_timestamp, Smarty $smarty)
{
    if ($resource_name !== 'foo.conf') {
        return 'no.conf';
    }

    return $smarty->getConfigDir(0) . 'test.conf';
}

/**
 * config handler returning false
 *
 * @param         $resource_type
 * @param         $resource_name
 * @param         $config_source
 * @param         $config_timestamp
 * @param \Smarty $smarty
 *
 * @return bool
 */
function configHandlerFalse($resource_type, $resource_name, &$config_source, &$config_timestamp, Smarty $smarty)
{
    return false;
}
