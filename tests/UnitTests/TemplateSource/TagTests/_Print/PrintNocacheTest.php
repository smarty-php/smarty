<?php
/**
 * Smarty PHPunit tests variable output with nocache attribute
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for variable output with nocache attribute tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PrintNocacheTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test print nocache caching disabled
     */
    public function testPrintNocacheCachingNo1()
    {
        $this->smarty->caching = 0;
        $this->smarty->assign('foo', 0);
        $this->smarty->assign('bar', 'A');
        $this->assertEquals("0A", $this->smarty->fetch('test_print_nocache.tpl'));
    }

    public function testPrintNocacheCachingNo2()
    {
        $this->smarty->caching = 0;
        $this->smarty->assign('foo', 2);
        $this->smarty->assign('bar', 'B');
        $this->assertEquals("2B", $this->smarty->fetch('test_print_nocache.tpl'));
    }

    /**
     * test print nocache caching enabled
     */
    public function testPrintNocacheCachingYes1()
    {
        $this->smarty->caching = 1;
        $this->smarty->assign('foo', 0);
        $this->smarty->assign('bar', 'A');
        $this->assertEquals("0A", $this->smarty->fetch('test_print_nocache.tpl'));
    }

    /**
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testPrintNocacheCachingYes2()
    {
        $this->smarty->caching = 1;
        $this->smarty->assign('foo', 2);
        $this->smarty->assign('bar', 'B');
        $this->assertEquals("2A", $this->smarty->fetch('test_print_nocache.tpl'));
    }
}
