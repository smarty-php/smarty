<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class MathTest extends PHPUnit_Smarty
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
     * test PHP function as modifier
     */
    public function testSyntax()
    {
        $this->smarty->disableSecurity();
        $expected = "20 -- 4";
        $tpl = $this->smarty->createTemplate('eval:{$x = 4}{$y = 5}{$x * $y} -- {20 / 5}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testFunction()
    {
        $this->smarty->disableSecurity();
        $expected = "20 -- 4";
        $tpl = $this->smarty->createTemplate('eval:{$x = 4}{$y = 5}{math equation="x * y" x=$x y=$y} -- {math equation="20 / 5"}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testSyntaxSin()
    {
        $this->smarty->disableSecurity();
        $expected = sin(4) . ' -- ' . sin(4);
        $tpl = $this->smarty->createTemplate('eval:{$x = 4}{$x|sin} -- {$y = sin($x)}{$y}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testFunctionSin()
    {
        $this->smarty->disableSecurity();
        $expected = sin(4) . ' -- ' . sin(4);
        $tpl = $this->smarty->createTemplate('eval:{$x = 4}{math equation="sin(x)" x=$x} -- {math equation="sin(x)" x=$x assign="y"}{$y}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testSyntaxFloat()
    {
        $this->smarty->disableSecurity();
        $expected = "22 -- 4.1";
        $tpl = $this->smarty->createTemplate('eval:{$x = 4}{$y = 5.5}{$x * $y} -- {20.5 / 5}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testFunctionFloat()
    {
        $this->smarty->disableSecurity();
        $expected = "22 -- 4.1";
        $tpl = $this->smarty->createTemplate('eval:{$x = 4}{$y = 5.5}{math equation="x * y" x=$x y=$y} -- {math equation="20.5 / 5"}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testSyntaxFormat()
    {
        $this->smarty->disableSecurity();
        $expected = "22.00 -- 4.10";
        $tpl = $this->smarty->createTemplate('eval:{$x = 4}{$y = 5.5}{$z = $x * $y}{"%0.2f"|sprintf:$z} -- {$x = 20.5}{$y = 5}{$z = $x / $y}{"%0.2f"|sprintf:$z}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testFunctionFormat()
    {
        $this->smarty->disableSecurity();
        $expected = "22.00 -- 4.10";
        $tpl = $this->smarty->createTemplate('eval:{$x = 4}{$y = 5.5}{math equation="x * y" x=$x y=$y format="%0.2f"} -- {math equation="20.5 / 5" format="%0.2f"}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testSyntaxString()
    {
        $this->smarty->disableSecurity();
        $expected = "22.00 -- 4.10";
        $tpl = $this->smarty->createTemplate('eval:{$x = "4"}{$y = "5.5"}{$z = $x * $y}{"%0.2f"|sprintf:$z} -- {$x = "20.5"}{$y = "5"}{$z = $x / $y}{"%0.2f"|sprintf:$z}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testFunctionString()
    {
        $this->smarty->disableSecurity();
        $expected = "22.00 -- 4.10";
        $tpl = $this->smarty->createTemplate('eval:{$x = "4"}{$y = "5.5"}{math equation="x * y" x=$x y=$y format="%0.2f"} -- {math equation="20.5 / 5" format="%0.2f"}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }
}
