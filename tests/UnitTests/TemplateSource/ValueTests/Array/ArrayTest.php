<?php
/**
 * Smarty PHPunit tests array definitions and access
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for array tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class ArrayTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test simple array definition
     */
    public function testSimpleArrayDefinition()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,3,4,5]}{foreach $foo as $bar}{$bar}{/foreach}');
        $this->assertEquals('12345', $this->smarty->fetch($tpl));
    }

    /**
     * test smarty2 array access
     */
    public function testSmarty2ArrayAccess()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,3,4,5]}{$foo.0}{$foo.1}{$foo.2}');
        $this->assertEquals('123', $this->smarty->fetch($tpl));
    }

    /**
     * test smarty3 array access
     */
    public function testSmarty3ArrayAccess()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,3,4,5]}{$foo[0]}{$foo[1]}{$foo[2]}');
        $this->assertEquals('123', $this->smarty->fetch($tpl));
    }

    /**
     * test indexed array definition
     */
    public function testIndexedArrayDefinition()
    {
        $tpl = $this->smarty->createTemplate('eval:{$x=\'d\'}{$foo=[a=>1,\'b\'=>2,"c"=>3,$x=>4]}{$foo[\'a\']}{$foo[\'b\']}{$foo[\'c\']}{$foo[\'d\']}');
        $this->assertEquals('1234', $this->smarty->fetch($tpl));
    }

    /**
     * test nested array
     */
    public function testNestedArray()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[a,b,c],4,5]}{$foo[2][1]}');
        $this->assertEquals('b', $this->smarty->fetch($tpl));
    }

    /**
     * test array math
     */
    public function testArrayMath1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{$foo[2][1]+1}');
        $this->assertEquals('9', $this->smarty->fetch($tpl));
    }

    public function testArrayMath2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{$foo.2.1+1}');
        $this->assertEquals('9', $this->smarty->fetch($tpl));
    }

    public function testArrayMath3()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{2+$foo[2][1]}');
        $this->assertEquals('10', $this->smarty->fetch($tpl));
    }

    public function testArrayMath4()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{2+$foo.2.1}');
        $this->assertEquals('10', $this->smarty->fetch($tpl));
    }

    public function testArrayMath5()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{$foo[2][0]+$foo[2][1]}');
        $this->assertEquals('15', $this->smarty->fetch($tpl));
    }

    public function testArrayMath6()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{$foo.2.0+$foo.2.1}');
        $this->assertEquals('15', $this->smarty->fetch($tpl));
    }

    public function testArrayVariableIndex1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{$x=2}{$y=0}{$foo.$x.$y}');
        $this->assertEquals('7', $this->smarty->fetch($tpl));
    }

    public function testArrayVariableIndex2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{$x=2}{$foo.$x.0}');
        $this->assertEquals('7', $this->smarty->fetch($tpl));
    }

    public function testArrayVariableIndex3()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{$x=0}{$foo.2.$x}');
        $this->assertEquals('7', $this->smarty->fetch($tpl));
    }

    public function testArrayVariableIndex4()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=[1,2,[7,8,9],4,5]}{$x=[1,0]}{$foo.2.{$x.1}}');
        $this->assertEquals('7', $this->smarty->fetch($tpl));
    }
}
