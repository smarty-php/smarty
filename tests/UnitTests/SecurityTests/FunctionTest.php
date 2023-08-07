<?php
/**
 * Smarty PHPunit tests of function calls
 *

 * @author  Uwe Tews
 */

/**
 * class for function tests
 *
 * 
 * 
 * 
 */
class FunctionTest extends PHPUnit_Smarty
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
     * test unknown function error
     */
    public function testUnknownFunction()
    {
        $this->smarty->enableSecurity();
        $this->expectException(\Smarty\CompilerException::class);
        $this->expectExceptionMessage('unknown modifier');
        $this->smarty->fetch('eval:{unknown()}');
    }
}
