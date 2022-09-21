<?php
/**
 * Smarty PHPunit tests {$smarty.post.foo}
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {$smarty.post.foo} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PostTest extends PHPUnit_Smarty
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
     * test $_POST
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider dataProvider
     */
    public function testPost($caching, $value) {
        $_POST['fooBar'] = $value;
        $this->smarty->caching = $caching;
        $this->assertEquals($value, $this->smarty->fetch('post.tpl'));
    }

    /**
     * test $_POST with modifier
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider dataProviderModifier
     */
    public function testPostModifier($caching, $value, $result) {
        $_POST['fooBar'] = $value;
        $this->smarty->caching = $caching;
        $this->assertEquals($result, $this->smarty->fetch('post_modifier.tpl'));
    }

    /**
     * test variable post
     *
     */
    public function testPostVariable() {
        $_POST['fooBarVar'] = 'fooBarVarValue';
        $this->smarty->assign('foo', 'fooBarVar');
        $this->assertEquals('fooBarVarValue', $this->smarty->fetch('post_variable.tpl'));
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
