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
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test {section} tag
     */
    public function testSection1()
    {
        $tpl = $this->smarty->createTemplate('section1.tpl');
        $tpl->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testSection2()
    {
        $tpl = $this->smarty->createTemplate('section2.tpl');
        $tpl->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testSection3()
    {
        $tpl = $this->smarty->createTemplate('section3.tpl');
        $this->assertEquals("else", $this->smarty->fetch($tpl));
    }

    public function testSection4()
    {
        $tpl = $this->smarty->createTemplate('section4.tpl');
        $tpl->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testSection6()
    {
        $tpl = $this->smarty->createTemplate('section6.tpl');
        $tpl->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789total10", $this->smarty->fetch($tpl));
    }

    public function testSection7()
    {
        $tpl = $this->smarty->createTemplate('section7.tpl');
        $tpl->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("011223344556677889910", $this->smarty->fetch($tpl));
    }

}
