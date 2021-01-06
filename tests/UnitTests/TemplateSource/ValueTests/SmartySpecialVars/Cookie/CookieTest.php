<?php
/**
 * Smarty PHPunit tests {$smarty.cookies.foo}
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for $smarty.cookies.foo} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CookieTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test cookies
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider dataProvider
     */
    public function testCookie($caching, $value) {
        $_COOKIE['fooBar'] = $value;
        $this->smarty->caching = $caching;
        $this->assertEquals($value, $this->smarty->fetch('cookie.tpl'));
    }
    /**
     * test variable cookies
     *
      */
    public function testCookieVariable() {
        $_COOKIE['fooBarVar'] = 'fooBarVarValue';
        $this->smarty->assign('foo', 'fooBarVar');
        $this->assertEquals('fooBarVarValue', $this->smarty->fetch('cookie_variable.tpl'));
    }

    /**
     * test cookies with modifier
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider dataProviderModifier
     */
    public function testCookieModifier($caching, $value, $result) {
        $_COOKIE['fooBar'] = $value;
        $this->smarty->caching = $caching;
        $this->assertEquals($result, $this->smarty->fetch('cookie_modifier.tpl'));
    }

    /**
     * data provider
     */
    public function dataProvider()
    {
        return array(
            'compile' => array(false, 'buh'),
            'compiled' => array(false, 'bar'),
            'create cache' => array(true, 'cached buh'),
            'cacheded' => array(true, 'cached bar'),
        );
    }
    public function dataProviderModifier()
    {
        return array(
            'compile' => array(false, 'buh', 3),
            'compiled' => array(false, 'bar1', 4),
            'create cache' => array(true, 'cached buh', 10),
            'cacheded' => array(true, 'cached bar1', 11),
        );
    }

}
