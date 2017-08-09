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
class AutoliteralTest extends PHPUnit_Smarty
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
     * test '{ ' delimiter
     */
    public function testSetAutoliteral()
    {
        $this->smarty->setAutoLiteral(true);
        $this->smarty->assign('i','foo');
        $this->assertEquals('{ $i}foo', $this->smarty->fetch('eval:{ $i}{$i}'));
    }

    public function testSetAutoliteral2()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->assign('i','foo');
        $this->assertEquals('foofoo', $this->smarty->fetch('eval:{ $i}{$i}'));
    }
    public function testSetAutoliteral3()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->assign('i','foo');
        $this->assertEquals('{{$i}foo', $this->smarty->fetch('eval:{{$i}{$i}'));
    }
    public function testSetAutoliteral4()
    {
        $this->smarty->setAutoLiteral(false);
        $this->smarty->assign('i','foo');
        $this->assertEquals('{{ $i}foo', $this->smarty->fetch('eval:{{ $i}{$i}'));
    }
    public function testSetAutoliteral5()
    {
        $this->smarty->setAutoLiteral(true);
        $this->smarty->assign('i','foo');
        $this->assertEquals('{ {{$i}foo', $this->smarty->fetch('eval:{ {{$i}{$i}'));
    }
}
