<?php
/**
 * Smarty PHPunit tests for cache resource file
 *

 * @author  Uwe Tews
 */

/**
 * class for cache resource file tests
 *
 * 
 * 
 * 
 */
class HttpModifiedSinceTest extends PHPUnit_Smarty
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
     *
     */
    public function testDisabled()
    {
        $_SERVER['SMARTY_PHPUNIT_DISABLE_HEADERS'] = true;
        $_SERVER['SMARTY_PHPUNIT_HEADERS'] = array();

        $this->smarty->setCacheModifiedCheck(false);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 20;
        ob_start();
        $this->smarty->display('helloworld.tpl');
        $output = ob_get_contents();
        ob_end_clean();
        $this->assertEquals('hello world', $output);
        $this->assertEquals('', join("\r\n", $_SERVER['SMARTY_PHPUNIT_HEADERS']));

        unset($_SERVER['HTTP_IF_MODIFIED_SINCE']);
        unset($_SERVER['SMARTY_PHPUNIT_HEADERS']);
        unset($_SERVER['SMARTY_PHPUNIT_DISABLE_HEADERS']);
    }

    /**
     *
     */
    public function testEnabledUncached()
    {
        $_SERVER['SMARTY_PHPUNIT_DISABLE_HEADERS'] = true;
        $_SERVER['SMARTY_PHPUNIT_HEADERS'] = array();

        $this->smarty->setCacheModifiedCheck(true);
        $this->smarty->caching = false;
        $this->smarty->cache_lifetime = 20;
        ob_start();
        $this->smarty->display('helloworld.tpl');
        $output = ob_get_contents();
        ob_end_clean();
        $this->assertEquals('hello world', $output);
        $this->assertEquals('', join("\r\n", $_SERVER['SMARTY_PHPUNIT_HEADERS']));

        unset($_SERVER['HTTP_IF_MODIFIED_SINCE']);
        unset($_SERVER['SMARTY_PHPUNIT_HEADERS']);
        unset($_SERVER['SMARTY_PHPUNIT_DISABLE_HEADERS']);
    }

    /**
     *
     */
    public function testEnabledCached()
    {
        $_SERVER['SMARTY_PHPUNIT_DISABLE_HEADERS'] = true;
        $_SERVER['SMARTY_PHPUNIT_HEADERS'] = array();

        $this->smarty->setCacheModifiedCheck(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 20;

        ob_start();
        $l1 = ob_get_level();
        $this->smarty->display('helloworld.tpl');
        $l2 = ob_get_level();
        $output = ob_get_contents();
        ob_end_clean();
        $this->assertEquals('hello world', $output);
        $t1 = time();
        $t2 = strtotime(substr( $_SERVER['SMARTY_PHPUNIT_HEADERS'][0],15));
        $this->assertTrue(($t2-$t1) <= 1);
    }

    /**
     *
     */
    public function testEnabledCached2()
    {

        $_SERVER['SMARTY_PHPUNIT_HEADERS'] = array();
        $_SERVER['HTTP_IF_MODIFIED_SINCE'] = gmdate('D, d M Y H:i:s', time() - 3600) . ' GMT';
        $this->smarty->setCacheModifiedCheck(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 20;
        ob_start();
        $l3 = ob_get_level();
        $this->smarty->display('helloworld.tpl');
        $l4 = ob_get_level();
        $output = ob_get_contents();
        ob_end_clean();
        $this->assertEquals('hello world', $output);
        $t1 = time();
        $t2 = strtotime(substr( $_SERVER['SMARTY_PHPUNIT_HEADERS'][0],15));
        $this->assertTrue(($t2-$t1) <= 1);
    }

    /**
     *
     */
    public function testEnabledCached3()
    {

        $_SERVER['SERVER_PROTOCOL'] = 'HTTP/1.1';
        $_SERVER['SMARTY_PHPUNIT_HEADERS'] = array();
        $_SERVER['HTTP_IF_MODIFIED_SINCE'] = gmdate('D, d M Y H:i:s', time() + 10) . ' GMT';
        $this->smarty->setCacheModifiedCheck(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 20;
        ob_start();
        $l5 = ob_get_level();
        $this->smarty->display('helloworld.tpl');
        $l6 = ob_get_level();
        $output = ob_get_contents();
        ob_end_clean();
        $this->assertEquals('', $output);
        $this->assertEquals('304 Not Modified', join("\r\n", $_SERVER['SMARTY_PHPUNIT_HEADERS']));

        unset($_SERVER['HTTP_IF_MODIFIED_SINCE']);
        unset($_SERVER['SMARTY_PHPUNIT_HEADERS']);
        unset($_SERVER['SMARTY_PHPUNIT_DISABLE_HEADERS']);
    }
}
