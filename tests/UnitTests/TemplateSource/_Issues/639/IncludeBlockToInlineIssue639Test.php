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
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 *
 * Switching from block to inline include crashes render
 */
class IncludeBlockToInlineIssue639Test extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testInlineFirst()
    {
        $this->assertEquals("include-inline\ninclude-regular\n", $this->smarty->fetch('001_inline_first.tpl'));
    }
    public function testInlineLast()
    {
        $this->assertEquals("include-regular\ninclude-inline\n", $this->smarty->fetch('002_inline_last.tpl'));
    }
}
