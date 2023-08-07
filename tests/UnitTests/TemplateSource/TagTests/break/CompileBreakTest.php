<?php
/**
 * Smarty PHPunit tests compilation of {break} tag
 *

 * @author  Uwe Tews
 */

/**
 * class for {break} tag tests
 *
 *
 *
 *
 */
class CompileBreakTest extends PHPUnit_Smarty
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
