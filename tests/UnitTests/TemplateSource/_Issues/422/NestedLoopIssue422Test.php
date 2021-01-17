<?php
/**
 * Smarty PHPunit tests compiler errors
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for compiler tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 *
 * Problem with total property of {section} and {foreach} in nested loop
 */
class NestedLoopIssue422Test extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testnested422()
    {
         $this->assertEquals('loop: 1inner: 0loop: 2inner: 1', $this->smarty->fetch('422_test.tpl'));
    }

}
