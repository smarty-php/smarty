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
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class AutoliteralTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../__shared/PHPunitplugins/");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test '{ ' delimiter
     */
    public function testSetAutoliteral()
    {
        $this->smarty->setAutoLiteral(true);
        $this->smarty->assign('i','foo');
        $this->assertEquals('{ $i}foo', $this->smarty->fetch('autoliteral.tpl'));
    }

    public function testSetAutoliteral2()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->setCompileId(1);
        $this->smarty->assign('i','bar');
        $this->assertEquals('barbar', $this->smarty->fetch('autoliteral.tpl'));
    }
    /**
     * test '{ ' delimiter in double quotes auto_literal true
     * @runInSeparateProcess
     *
     */
    public function testSetAutoliteralDoublequote()
    {
        $this->smarty->setAutoLiteral(true);
        $this->assertEquals(' output: double { counter} 1 quote', $this->smarty->fetch('autoliteraldoublequote.tpl'));
    }
    /**
     * test '{ ' delimiter in double quotes auto_literal false
     * @runInSeparateProcess
     *
     */
    public function testSetAutoliteralDoublequote2()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->setCompileId(1);
        $this->assertEquals(' output: double 1 2 quote', $this->smarty->fetch('autoliteraldoublequote.tpl'));
    }

    /**
     * test '{{ ' delimiter in double quotes auto_literal true
     * @runInSeparateProcess
     *
     */
    public function testSetAutoliteralDoublequote3()
    {
        $this->smarty->setAutoLiteral(true);
        $this->assertEquals(' output: double {{ counter} {{ counter}}  quote', $this->smarty->fetch('autoliteraldoublequote2.tpl'));
    }

    public function testSetAutoliteralBlock()
    {
        $this->smarty->setAutoLiteral(true);
        $this->assertEquals('{ dummyblock}foo{ /dummyblock}', $this->smarty->fetch('autoliteralblock.tpl'));
    }
    public function testSetAutoliteralBlock1()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->setCompileId(1);
        $this->assertEquals('foo', $this->smarty->fetch('autoliteralblock.tpl'));
    }

}
