<?php

/**
 * Smarty PHPunit tests of constants
 *

 * @author                 Uwe Tews
 * 
 * 
 * 
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
    public function setUp(): void
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

    public function testConstantsUndefined()
    {
        $tpl = $this->smarty->createTemplate('string:{$smarty.const.MYCONSTANT2}');
        $this->assertEquals("", $this->smarty->fetch($tpl));
    }

    public function testConstantsUndefined2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo = MYCONSTANT2}{$foo}');
        $this->assertEquals("MYCONSTANT2", $this->smarty->fetch($tpl));
    }

    public function testConstantsUndefined3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if $smarty.const.MYCONSTANT2}{$smarty.const.MYCONSTANT2}{/if}');
        $this->assertEquals("", $this->smarty->fetch($tpl));
    }
}
