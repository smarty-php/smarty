<?php
/**
 * Smarty PHPunit tests {$smarty.constant.foo}
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {$smarty.constant.foo} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class SmartyConstantTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test {$smarty.constant.foo}
     *
     */
    public function testSmartyConstant() {
        define('MY_CONST_VAL','MyConstant');

        $this->assertEquals('MyConstant', $this->smarty->fetch('constant.tpl'));
    }
    /**
     * test {$smarty.constant.foo}
     *
     */
    public function testSmartyConstantVariable() {
        define('MY_CONST_VAL2','MyConstantVar');
        $this->smarty->assign('foo', 'MY_CONST_VAL2');
        $this->assertEquals('MyConstantVar', $this->smarty->fetch('constant_variable.tpl'));
    }
 }
