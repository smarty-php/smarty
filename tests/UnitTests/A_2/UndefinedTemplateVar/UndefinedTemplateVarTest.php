<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 */

/**
 * class for protected $template_dir, $compile_dir, $cache_dir, $config_dir, $plugins_dir property tests
 *
 * 
 */
class UndefinedTemplateVarTest extends PHPUnit_Smarty
{
    /*
     * Setup test fixture
     */
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
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
		$this->smarty->error_unassigned = true;
        $this->expectException(PHPUnit\Framework\Error\Error::class);
		$this->expectExceptionMessage('Undefined ');
        $e1 = error_reporting();
        $this->assertEquals('undefined = ', $this->smarty->fetch('001_main.tpl'));
        $e2 = error_reporting();
        $this->assertEquals($e1, $e2);
    }

    public function testUndefinedSimpleVar() {
        $this->smarty->muteUndefinedOrNullWarnings();
        $tpl = $this->smarty->createTemplate('string:a{if $undef}def{/if}b');
        $this->assertEquals("ab", $this->smarty->fetch($tpl));
    }

    public function testUndefinedArrayIndex() {
        $this->smarty->muteUndefinedOrNullWarnings();
        $tpl = $this->smarty->createTemplate('string:a{if $ar.undef}def{/if}b');
        $tpl->assign('ar', []);
        $this->assertEquals("ab", $this->smarty->fetch($tpl));
    }

    public function testUndefinedArrayIndexDeep() {
        $this->smarty->muteUndefinedOrNullWarnings();
        $tpl = $this->smarty->createTemplate('string:a{if $ar.undef.nope.neither}def{/if}b');
        $tpl->assign('ar', []);
        $this->assertEquals("ab", $this->smarty->fetch($tpl));
    }

    public function testUndefinedArrayIndexError()
    {
        $exceptionThrown = false;

        try {
            $tpl = $this->smarty->createTemplate('string:a{if $ar.undef}def{/if}b');
            $tpl->assign('ar', []);
            $this->smarty->fetch($tpl);
        } catch (Exception $e) {

            $exceptionThrown = true;
            $this->assertStringStartsWith('Undefined ', $e->getMessage());
            $this->assertTrue(in_array(
                get_class($e),
                [
                    'PHPUnit\Framework\Error\Warning',
                    'PHPUnit\Framework\Error\Notice',
                ]
            ));
        }
        $this->assertTrue($exceptionThrown);
    }

    public function testUsingNullAsAnArrayIsMuted() {
        $this->smarty->setErrorReporting(E_ALL);
        $this->smarty->muteUndefinedOrNullWarnings();
        $tpl = $this->smarty->createTemplate('string:a{if $undef.k}def{/if}b');
        $this->assertEquals("ab", $this->smarty->fetch($tpl));
    }

    public function testUsingFalseAsAnArrayIsMuted() {
        $this->smarty->setErrorReporting(E_ALL);
        $this->smarty->muteUndefinedOrNullWarnings();
        $tpl = $this->smarty->createTemplate('string:a{if $nottrue.k}def{/if}b');
        $this->smarty->assign('nottrue', false);
        $this->assertEquals("ab", $this->smarty->fetch($tpl));
    }


	public function testDereferenceOnNull() {
		$this->smarty->setErrorReporting(E_ALL & ~E_WARNING & ~E_NOTICE);
		$this->smarty->muteUndefinedOrNullWarnings();
		$tpl = $this->smarty->createTemplate('string:a{if $object->myprop}def{/if}b');
		$this->smarty->assign('object', null);
		$this->assertEquals("ab", $this->smarty->fetch($tpl));
	}


	public function testDereferenceOnBool() {
		$this->smarty->setErrorReporting(E_ALL & ~E_NOTICE);
		$this->smarty->muteUndefinedOrNullWarnings();
		$tpl = $this->smarty->createTemplate('string:a{if $object->myprop}def{/if}b');
		$this->smarty->assign('object', false);
		$this->assertEquals("ab", $this->smarty->fetch($tpl));
	}


	public function testDereferenceOnString() {
		$this->smarty->setErrorReporting(E_ALL & ~E_NOTICE);
		$this->smarty->muteUndefinedOrNullWarnings();
		$tpl = $this->smarty->createTemplate('string:a{if $object->myprop}def{/if}b');
		$this->smarty->assign('object', 'xyz');
		$this->assertEquals("ab", $this->smarty->fetch($tpl));
	}

}
