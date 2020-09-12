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
     * Test Error suppression template fetched by Smarty object
     */
    public function testErrorDisabled()
    {
        $e1 = error_reporting();
        $this->smarty->setErrorReporting(E_ALL & ~E_WARNING & ~E_NOTICE);
        $this->assertEquals('undefined = ', $this->smarty->fetch('001_main.tpl'));
        $e2 = error_reporting();
        $this->assertEquals($e1, $e2);
    }

    /**
     * Test Error suppression template fetched by template object
     */
    public function testErrorDisabledTplObject_1()
    {
        $e1 = error_reporting();
        $this->smarty->setErrorReporting(E_ALL & ~E_WARNING & ~E_NOTICE);
        $tpl = $this->smarty->createTemplate('001_main.tpl');
        $this->assertEquals('undefined = ', $tpl->fetch());
        $e2 = error_reporting();
        $this->assertEquals($e1, $e2);
    }

	/**
	 * Test Error suppression template object fetched by Smarty object
	 */
    public function testErrorDisabledTplObject_2()
    {
        $e1 = error_reporting();
        $this->smarty->setErrorReporting(E_ALL & ~E_WARNING & ~E_NOTICE);
        $tpl = $this->smarty->createTemplate('001_main.tpl');
        $this->assertEquals('undefined = ', $this->smarty->fetch($tpl));
        $e2 = error_reporting();
        $this->assertEquals($e1, $e2);
    }

    /**
     * Throw Error message
     */
    public function testError()
    {
	    $exceptionThrown = false;

        try {
	        $e1 = error_reporting();
	        $this->assertEquals('undefined = ', $this->smarty->fetch('001_main.tpl'));
	        $e2 = error_reporting();
	        $this->assertEquals($e1, $e2);
        } catch (Exception $e) {

        	$exceptionThrown = true;
	        $this->assertStringStartsWith('Undefined ', $e->getMessage());
	        $this->assertTrue(in_array(
	        	get_class($e),
		        array(
		        	'PHPUnit_Framework_Error_Warning',
			        'PHPUnit_Framework_Error_Notice',
			        'PHPUnit\Framework\Error\Warning',
			        'PHPUnit\Framework\Error\Notice',
		        )
	        ));
        }
	    $this->assertTrue($exceptionThrown);
    }
}
