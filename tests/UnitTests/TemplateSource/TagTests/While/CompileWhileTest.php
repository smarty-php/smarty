<?php
/**
 * Smarty PHPunit tests compilation of {while} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {while} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileWhileTest extends PHPUnit_Smarty
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
     * test {while 'condition'} tag
     */
    public function testWhileCondition()
    {
        $tpl = $this->smarty->createTemplate('eval:{$x=0}{while $x<10}{$x}{$x=$x+1}{/while}');
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    /**
     * test {while 'statement'} tag
     */
    public function testWhileStatement()
    {
        $tpl = $this->smarty->createTemplate('eval:{$y=5}{while $y=$y-1}{$y}{/while}');
        $this->assertEquals("4321", $this->smarty->fetch($tpl));
    }
}
