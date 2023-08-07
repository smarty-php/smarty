<?php
/**
 * Smarty PHPunit tests static class access to constants, variables and methods
 *

 * @author  Uwe Tews
 */

/**
 * class for static class access to constants, variables and methods tests
 *
 * 
 * 
 * 
 */
class StaticClassAccessTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->disableSecurity();
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test static class variable
     */
    public function testStaticClassVariable()
    {
        $tpl = $this->smarty->createTemplate('eval:{mystaticclass::$static_var}');
        $this->assertEquals('5', $this->smarty->fetch($tpl));
    }

    /**
     * test registered static class variable
     */
    public function testStaticRegisteredClassVariable()
    {
        $this->smarty->registerClass('registeredclass', 'mystaticclass');
        $tpl = $this->smarty->createTemplate('eval:{registeredclass::$static_var}');
        $this->assertEquals('5', $this->smarty->fetch($tpl));
    }

    /**
     * test static class constant
     */
    public function testStaticClassConstant()
    {
        $tpl = $this->smarty->createTemplate('eval:{mystaticclass::STATIC_CONSTANT_VALUE}');
        $this->assertEquals('3', $this->smarty->fetch($tpl));
    }

    /**
     * test static class constant
     */
    public function testRegisteredStaticClassConstant()
    {
        $this->smarty->registerClass('registeredclass', 'mystaticclass');
        $tpl = $this->smarty->createTemplate('eval:{registeredclass::STATIC_CONSTANT_VALUE}');
        $this->assertEquals('3', $this->smarty->fetch($tpl));
    }

    /**
     * test static class method
     */
    public function testStaticClassmethod()
    {
        $tpl = $this->smarty->createTemplate('eval:{mystaticclass::square(5)}');
        $this->assertEquals('25', $this->smarty->fetch($tpl));
    }

    /**
     * test static class method
     */
    public function testRegisteredStaticClassmethod()
    {
        $this->smarty->registerClass('registeredclass', 'mystaticclass');
        $tpl = $this->smarty->createTemplate('eval:{registeredclass::square(5)}');
        $this->assertEquals('25', $this->smarty->fetch($tpl));
    }

    /**
     * test static class variable method
     */
    public function testStaticClassVariablemethod()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'square\'}{mystaticclass::$foo(5)}');
        $this->assertEquals('25', $this->smarty->fetch($tpl));
    }

    /**
     * test registered static class variable method
     */
    public function testRegisteredStaticClassVariablemethod()
    {
        $this->smarty->registerClass('registeredclass', 'mystaticclass');
        $tpl = $this->smarty->createTemplate('eval:{$foo=\'square\'}{registeredclass::$foo(5)}');
        $this->assertEquals('25', $this->smarty->fetch($tpl));
    }

    /**
     * test static class variable method
     */
    public function testStaticClassVariablemethod2()
    {
        $tpl = $this->smarty->createTemplate('eval:{mystaticclass::$foo(5)}');
        $tpl->assign('foo', 'square');
        $this->assertEquals('25', $this->smarty->fetch($tpl));
    }

    /**
     * test registered static class variable method
     */
    public function testRegisteredStaticClassVariablemethod2()
    {
        $this->smarty->registerClass('registeredclass', 'mystaticclass');
        $tpl = $this->smarty->createTemplate('eval:{registeredclass::$foo(5)}');
        $tpl->assign('foo', 'square');
        $this->assertEquals('25', $this->smarty->fetch($tpl));
    }
}

class mystaticclass
{
    const STATIC_CONSTANT_VALUE = 3;
    static $static_var = 5;

    static function square($i)
    {
        return $i * $i;
    }
}
