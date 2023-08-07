<?php
/**
 * Smarty PHPUnit tests default config handler
 *

 * @author  Uwe Tews
 */

use Smarty\Smarty;

/**
 * class for default config handler test
 *
 * 
 * @preserveGlobalState    disabled
 * 
 */
class DefaultConfigHandlerTest extends PHPUnit_Smarty
{

    /**
     * Sets up the fixture
     * This method is called before a test is executed.
     *
     */
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->setForceCompile(true);
    }

    /**
     * test unknown config file
     */
    public function testUnknownConfigFile()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('Unable to load \'file:foo.conf\'');
        $this->smarty->configLoad('foo.conf');
    }

    /**
     * test register unknown default config handler
     */
    public function testRegisterUnknownDefaultConfigHandler()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('Default config handler');
        $this->expectExceptionMessage('not callable');
        $this->smarty->registerDefaultConfigHandler('foo');
    }

    /**
     * test default config handler replacement (config data)
     *
     * @throws \Exception
     * @throws \Smarty\Exception
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
     * @throws \Smarty\Exception
     */
    public function testDefaultConfigHandlerReplacementByConfigFile()
    {
        $this->smarty->registerDefaultConfigHandler('configHandlerFile');
        $this->smarty->configLoad('foo.conf');
        $this->assertEquals("123.4", $this->smarty->fetch('number.tpl'));
    }

    public function testDefaultConfigHandlerReplacementByConfigFileFail()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage("Unable to load default file 'no.conf' for 'file:fo.conf'");
        $this->smarty->registerDefaultConfigHandler('configHandlerFile');
        $this->smarty->configLoad('fo.conf');
        $this->assertEquals("123.4", $this->smarty->fetch('number.tpl'));
    }

    /**
     * test default config handler replacement (return false)
     */
    public function testDefaultConfigHandlerReplacementReturningFalse()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('Unable to load \'file:foo.conf\'');
        $this->smarty->configLoad('foo.conf');
    }

    /**
     * test default config handler replacement (return false)
     */
    public function testDefaultConfigHandlerReplacementReturningFalse1()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('No config default content for \'file:bla.conf\'');
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
function configHandlerData($resource_type, $resource_name, &$config_source, &$config_timestamp, \Smarty\Smarty $smarty)
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
function configHandlerFile($resource_type, $resource_name, &$config_source, &$config_timestamp, \Smarty\Smarty $smarty)
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
function configHandlerFalse($resource_type, $resource_name, &$config_source, &$config_timestamp, \Smarty\Smarty $smarty)
{
    return false;
}
