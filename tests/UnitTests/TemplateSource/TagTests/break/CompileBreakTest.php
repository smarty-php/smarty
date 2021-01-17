<?php
/**
 * Smarty PHPunit tests compilation of {break} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {break} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileBreakTest extends PHPUnit_Smarty
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
     * test {break} in foreach
     */
    public function testBreakForeach()
    {
        $this->smarty->assign('array', array(1,2,3));
        $this->assertEquals('1', $this->smarty->fetch('break_foreach.tpl'));
    }
    /**
     * test {break} in foreach nocache
     */
    public function testBreakForeachNocache()
    {
        $this->smarty->assign('array', array(1,2,3), true);
        $this->smarty->caching = true;
        $this->assertEquals('1', $this->smarty->fetch('break_foreach.tpl'));
    }
}
