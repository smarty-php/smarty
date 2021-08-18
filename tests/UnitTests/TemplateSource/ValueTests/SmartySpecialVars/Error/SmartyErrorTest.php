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
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test undefined Smarty special variable
     */
    public function testSmartyError() {
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('$smarty.foo is not defined');
        $this->assertEquals(Smarty::SMARTY_VERSION, $this->smarty->fetch('error.tpl'));
    }
 }
