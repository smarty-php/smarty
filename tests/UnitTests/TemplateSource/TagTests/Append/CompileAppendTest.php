<?php
/**
 * Smarty PHPunit tests compilation of append tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for append tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileAppendTest extends PHPUnit_Smarty
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
     * test aappand tag
     */
    public function testAppend1()
    {
        $this->assertEquals("12", $this->smarty->fetch('eval:{$foo=1}{append var=foo value=2}{foreach $foo as $bar}{$bar}{/foreach}'));
    }

    public function testAppend2()
    {
        $this->smarty->assign('foo', 1);
        $this->assertEquals("12", $this->smarty->fetch('eval:{append var=foo value=2}{foreach $foo as $bar}{$bar}{/foreach}'));
    }
}
