<?php
/**
 * Smarty PHPunit tests undefined Smarty special variable
 *

 * @author  Uwe Tews
 */

/**
 * class for undefined Smarty special variable tests
 *
 * 
 * 
 * 
 */
class SmartyErrorTest extends PHPUnit_Smarty
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
     * test undefined Smarty special variable
     */
    public function testSmartyError() {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('$smarty.foo is not defined');
        $this->assertEquals(\Smarty\Smarty::SMARTY_VERSION, $this->smarty->fetch('error.tpl'));
    }
 }
