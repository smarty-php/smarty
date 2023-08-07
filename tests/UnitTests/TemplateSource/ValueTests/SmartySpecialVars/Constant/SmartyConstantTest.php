<?php
/**
 * Smarty PHPunit tests {$smarty.constant.foo}
 *

 * @author  Uwe Tews
 */

/**
 * class for {$smarty.constant.foo} tests
 *
 * 
 * 
 * 
 */
class SmartyConstantTest extends PHPUnit_Smarty
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
