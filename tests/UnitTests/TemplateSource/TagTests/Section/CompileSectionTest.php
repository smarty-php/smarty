<?php
/**
 * Smarty PHPunit tests compilation of {section} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {section} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileSectionTest extends PHPUnit_Smarty
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
     * test {section} tag
     */
    public function testSection_001()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch('001_section.tpl'));
    }

    public function testSection_002()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch('002_section.tpl'));
    }

    public function testSection_003()
    {
        $this->assertEquals("else", $this->smarty->fetch('003_section.tpl'));
    }

    public function testSection_004()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch('004_section.tpl'));
    }

    public function testSection_006()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789total10", $this->smarty->fetch('006_section.tpl'));
    }

    public function testSection_007()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("011223344556677889910", $this->smarty->fetch('007_section.tpl'));
    }

}
