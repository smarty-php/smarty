<?php
/**
 * Smarty PHPunit tests of filter
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for filter tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class MuteExpectedErrorsTest extends PHPUnit_Smarty
{
    protected $_errors = array();

    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    public function error_handler($errno, $errstr, $errfile, $errline, $errcontext = array())
    {
        $this->_errors[] = $errfile . ' line ' . $errline;
    }

    public function testMuted()
    {
        set_error_handler(array($this, 'error_handler'));
        Smarty::muteExpectedErrors();

        $this->smarty->clearCache('default.tpl');
        $this->smarty->clearCompiledTemplate('default.tpl');
        $this->smarty->fetch('default.tpl');

        $this->assertEquals($this->_errors, array());

        @filemtime('ckxladanwijicajscaslyxck');
        $error = array(__FILE__ . ' line ' . (__LINE__ - 1));
        $this->assertEquals($this->_errors, $error);

        Smarty::unmuteExpectedErrors();
        restore_error_handler();
    }

    /**
     *
     * @rrunInSeparateProcess
     *
     */
    public function testUnmuted()
    {
        set_error_handler(array($this, 'error_handler'));

        $this->smarty->clearCache('default.tpl');
        $this->smarty->clearCompiledTemplate('default.tpl');
        $this->smarty->fetch('default.tpl');

        $this->assertEquals(Smarty::$_IS_WINDOWS ? 2 : 2, count($this->_errors));

        @filemtime('ckxladanwijicajscaslyxck');
        $this->assertEquals(Smarty::$_IS_WINDOWS ? 3 : 3, count($this->_errors));

        restore_error_handler();
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testMutedCaching()
    {
        set_error_handler(array($this, 'error_handler'));
        Smarty::muteExpectedErrors();

        $this->smarty->caching = true;
        $this->smarty->clearCache('default.tpl');
        $this->smarty->clearCompiledTemplate('default.tpl');
        $this->smarty->fetch('default.tpl');

        $this->assertEquals($this->_errors, array());

        @filemtime('ckxladanwijicajscaslyxck');
        $error = array(__FILE__ . ' line ' . (__LINE__ - 1));
        $this->assertEquals($error, $this->_errors);

        Smarty::unmuteExpectedErrors();
        restore_error_handler();
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testUnmutedCaching()
    {
        set_error_handler(array($this, 'error_handler'));

        $this->smarty->caching = true;
        $this->smarty->clearCache('default.tpl');
        $this->smarty->clearCompiledTemplate('default.tpl');
        $this->smarty->fetch('default.tpl');

        $this->assertEquals(Smarty::$_IS_WINDOWS ? 2 : 2, count($this->_errors));

        @filemtime('ckxladanwijicajscaslyxck');
        $error = array(__FILE__ . ' line ' . (__LINE__ - 1));
        $this->assertEquals(Smarty::$_IS_WINDOWS ? 3 : 3, count($this->_errors));

        restore_error_handler();
    }
}
