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

    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        set_error_handler(array($this, 'error_handler'));
    }

    public function tearDown(): void {
        restore_error_handler();
        parent::tearDown();
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

        Smarty::muteExpectedErrors();

        $this->smarty->clearCache('default.tpl');
        $this->smarty->clearCompiledTemplate('default.tpl');
        $this->smarty->fetch('default.tpl');

        $this->assertEquals($this->_errors, array());

        @filemtime('ckxladanwijicajscaslyxck');
        $error = array(__FILE__ . ' line ' . (__LINE__ - 1));
        $this->assertEquals($this->_errors, $error);

        Smarty::unmuteExpectedErrors();
    }

    /**
     *
     * @rrunInSeparateProcess
     *
     */
    public function testUnmuted()
    {

        $this->smarty->clearCache('default.tpl');
        $this->smarty->clearCompiledTemplate('default.tpl');
        $this->smarty->fetch('default.tpl');

        $this->assertEquals($this->_errors, array());

        @filemtime('ckxladanwijicajscaslyxck');
        $this->assertEquals(1, count($this->_errors));

    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testMutedCaching()
    {
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
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testUnmutedCaching()
    {

        $this->smarty->caching = true;
        $this->smarty->clearCache('default.tpl');
        $this->smarty->clearCompiledTemplate('default.tpl');
        $this->smarty->fetch('default.tpl');

        $this->assertEquals($this->_errors, array());

        @filemtime('ckxladanwijicajscaslyxck');
        $this->assertEquals(1, count($this->_errors));

        restore_error_handler();
    }
}
