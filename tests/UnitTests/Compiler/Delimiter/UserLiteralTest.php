<?php
/**
 * Smarty PHPunit tests of delimiter
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for delimiter tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class UserliteralTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        if (!property_exists('Smarty', 'literals')) {
            $this->markTestSkipped('user literal support');
        } else {
            $this->setUpSmarty(dirname(__FILE__));
        }
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testUserLiteral()
    {
        $this->smarty->setAutoLiteral(true);
        $this->assertEquals('{{ 1 }}', $this->smarty->fetch('userliteral.tpl'));
    }
    public function testUserLiteral1()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->setCompileId(1);
        $this->assertEquals('1', $this->smarty->fetch('userliteral.tpl'));
    }
    public function testUserLiteral2()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->setLiterals(array('{{','}}'));
        $this->assertEquals('{{1}}', $this->smarty->fetch('userliteral1.tpl'));
    }
    public function testUserLiteral3()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->setLeftDelimiter('<-');
        $this->smarty->setRightDelimiter('->');
        $this->smarty->setLiterals(array('<--','-->'));
        $this->assertEquals('1 <--1-->', $this->smarty->fetch('userliteral2.tpl'));
    }
    public function testUserLiteral4()
    {
        $this->smarty->setAutoLiteral(true);
        $this->smarty->setLeftDelimiter('<-');
        $this->smarty->setRightDelimiter('->');
        $this->smarty->setCompileId(1);
        $this->smarty->setLiterals(array('<--','-->'));
        $this->assertEquals('<- 1 -> <--1-->', $this->smarty->fetch('userliteral2.tpl'));
    }
    public function testUserLiteral5()
    {
        $this->smarty->setAutoLiteral(true);
        $this->smarty->setLiterals(array('{%'));
        $this->assertEquals(' output: double {%counter} quote', $this->smarty->fetch('userliteraldoublequote.tpl'));
    }
}
