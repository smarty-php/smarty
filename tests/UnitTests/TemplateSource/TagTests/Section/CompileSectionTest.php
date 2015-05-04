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
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=[0,1,2,3,4,5,6,7,8,9]}{section name=bar loop=$foo}{$foo[bar]}{/section}');
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testSection2()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=[0,1,2,3,4,5,6,7,8,9]}{section name=bar loop=$foo}{$foo[bar]}{sectionelse} else {/section}');
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testSection3()
    {
        $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
        $tpl = $this->smarty->createTemplate('eval:{section name=bar loop=$foo}{$foo[bar]}{sectionelse}else{/section}');
        $this->assertEquals("else", $this->smarty->fetch($tpl));
    }

    public function testSection4()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=[0,1,2,3,4,5,6,7,8,9]}{section name=bar loop=$foo}{$foo[bar]}{sectionelse} else {/section}');
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testSection6()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=[0,1,2,3,4,5,6,7,8,9]}{section name=bar loop=$foo}{$foo[bar]}{sectionelse} else {/section}total{$smarty.section.bar.total}');
        $this->assertEquals("0123456789total10", $this->smarty->fetch($tpl));
    }

    public function testSection7()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=[0,1,2,3,4,5,6,7,8,9]}{section name=bar loop=$foo}{$smarty.section.bar.index}{$smarty.section.bar.iteration}{sectionelse} else {/section}');
        $this->assertEquals("011223344556677889910", $this->smarty->fetch($tpl));
    }
}
