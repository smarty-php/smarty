<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 */

/**
 * class for protected $template_dir, $compile_dir, $cache_dir, $config_dir, $plugins_dir property tests
 *
 * @backupStaticAttributes enabled
 */
class UndefinedTemplateVarTest extends PHPUnit_Smarty
{
    /*
     * Setup test fixture
     */
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        error_reporting(E_ALL | E_STRICT);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * Test E_NOTICE suppression template fetched by Smarty object
     */
    public function testE_NoticeDisabled()
    {
        $e1 = error_reporting();
        $this->smarty->setErrorReporting(E_ALL & ~E_NOTICE);
        $this->assertEquals('undefined = ', $this->smarty->fetch('001_main.tpl'));
        $e2 = error_reporting();
        $this->assertEquals($e1, $e2);
    }

    /**
     * Test E_NOTICE suppression template fetched by template object
     */
    public function testE_NoticeDisabledTplObject_1()
    {
        $e1 = error_reporting();
        $this->smarty->setErrorReporting(E_ALL & ~E_NOTICE);
        $tpl = $this->smarty->createTemplate('001_main.tpl');
        $this->assertEquals('undefined = ', $tpl->fetch());
        $e2 = error_reporting();
        $this->assertEquals($e1, $e2);
    }

    public function testE_NoticeDisabledTplObject_2()
    {
        $e1 = error_reporting();
        $this->smarty->setErrorReporting(E_ALL & ~E_NOTICE);
        $tpl = $this->smarty->createTemplate('001_main.tpl');
        $this->assertEquals('undefined = ', $this->smarty->fetch($tpl));
        $e2 = error_reporting();
        $this->assertEquals($e1, $e2);
    }

    /**
     * Throw E_NOTICE message
     *
     * @expectedException PHPUnit_Framework_Error_Notice
     * @expectedExceptionMessage Undefined index: foo
     */
    public function testE_Notice()
    {
            $e1 = error_reporting();
            $this->assertEquals('undefined = ', $this->smarty->fetch('001_main.tpl'));
            $e2 = error_reporting();
            $this->assertEquals($e1, $e2);
    }
}
