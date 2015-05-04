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
}
