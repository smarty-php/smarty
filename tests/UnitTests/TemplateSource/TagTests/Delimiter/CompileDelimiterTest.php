<?php
/**
 * Smarty PHPunit tests compilation of delimiter tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for delimiter tags tests
 *
 * @run TestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileDelimiterTest extends PHPUnit_Smarty
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

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage nocache option not allowed
     */
    public function testLeftDelimiterError()
    {
        $tpl = $this->smarty->createTemplate('string:x{ldelim nocache}x');
        $this->assertEquals('x{x', $this->smarty->fetch($tpl));
    }


}
