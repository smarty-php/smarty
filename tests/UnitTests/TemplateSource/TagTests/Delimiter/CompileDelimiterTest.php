<?php
/**
 * Smarty PHPunit tests compilation of delimiter tags
 *

 * @author  Uwe Tews
 */

/**
 * class for delimiter tags tests
 *
 * @run TestsInSeparateProcess
 * 
 * 
 */
class CompileDelimiterTest extends PHPUnit_Smarty
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
     * test delimiter tag test
     */
     public function testLeftDelimiter()
    {
        $tpl = $this->smarty->createTemplate('string:x{ldelim}x');
        $this->assertEquals('x{x', $this->smarty->fetch($tpl));
    }

    public function testRightDelimiter()
    {
        $tpl = $this->smarty->createTemplate('string:x{rdelim}x');
        $this->assertEquals('x}x', $this->smarty->fetch($tpl));
    }

    public function testLeftDelimiterError()
    {
        $this->expectException(\Smarty\CompilerException::class);
        $this->expectExceptionMessage('nocache option not allowed');
        $tpl = $this->smarty->createTemplate('string:x{ldelim nocache}x');
        $this->assertEquals('x{x', $this->smarty->fetch($tpl));
    }


}
