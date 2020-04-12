<?php
/**
 * Smarty PHPunit tests undefined Smarty special variable
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for undefined Smarty special variable tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class SmartyErrorTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage $smarty.foo is not defined
     * test undefined Smarty special variable
     *
     */
    public function testSmartyError() {
        $this->assertEquals(Smarty::SMARTY_VERSION, $this->smarty->fetch('error.tpl'));
    }
 }
