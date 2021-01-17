<?php
/**
 * Smarty PHPunit tests of function calls
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for function tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class FunctionTest extends PHPUnit_Smarty
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
     * test unknown function error
     */
    public function testUnknownFunction()
    {
        $this->smarty->enableSecurity();
        try {
            $this->smarty->fetch('eval:{unknown()}');
        }
        catch (Exception $e) {
            $this->assertStringContainsString("PHP function 'unknown' not allowed by security setting", $e->getMessage());

            return;
        }
        $this->fail('Exception for unknown function has not been raised.');
    }
}
