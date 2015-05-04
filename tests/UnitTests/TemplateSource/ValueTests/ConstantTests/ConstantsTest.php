<?php

/**
 * Smarty PHPunit tests of constants
 *
 * @package                PHPunit
 * @author                 Uwe Tews
 * @backupStaticAttributes enabled
 */
Class TestConst
{
    const CONSTVAL = 'okay';
}

/**
 * class for constants tests
 */
class ConstantsTest extends PHPUnit_Smarty
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
     * test constants
     */
    public function testConstants()
    {
        define('MYCONSTANTS', 'hello world');
        $tpl = $this->smarty->createTemplate('eval:{$smarty.const.MYCONSTANTS}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    public function testConstants2()
    {
        $tpl = $this->smarty->createTemplate('eval:{MYCONSTANTS}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    public function testConstants3()
    {
        $tpl = $this->smarty->createTemplate('eval:{$x=MYCONSTANTS}{$x}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    public function testConstants4()
    {
        $tpl = $this->smarty->createTemplate('eval:{TestConst::CONSTVAL}');
        $this->assertEquals("okay", $this->smarty->fetch($tpl));
    }

    public function testConstants5()
    {
        $tpl = $this->smarty->createTemplate('eval:{if TestConst::CONSTVAL == "okay"}yes{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testConstants6()
    {
        $tpl = $this->smarty->createTemplate('eval:{$obj::CONSTVAL}');
        $tpl->assign('obj', new TestConst());
        $this->assertEquals("okay", $this->smarty->fetch($tpl));
    }

    public function testConstants7()
    {
        $tpl = $this->smarty->createTemplate('eval:{if $obj::CONSTVAL == "okay"}yes{/if}');
        $tpl->assign('obj', new TestConst());
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }
}
