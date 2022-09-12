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
    public function setUp(): void
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

    public function testMultipleOperators()
    {
        $this->smarty->disableSecurity();
        $expected = "2 -- 2";
        $tpl = $this->smarty->createTemplate('eval:{$x = 5}{$y = 4}{math equation="x - y + 1" x=$x y=$y} -- {math equation="5 - 4 + 1"}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testMathMaxFunctionParameters()
    {
        $this->smarty->disableSecurity();
        $expected = max(0, 2) . ' -- ' . max(0, 2, 3);
        $tpl = $this->smarty->createTemplate('eval:{$x = 0}{$y = 2}{$z = 3}{math equation="max(x, y)" x=$x y=$y} -- {math equation="max(x, y, z)" x=$x y=$y z=$z}');
        $this->assertEquals($expected, $this->smarty->fetch($tpl));
    }

    public function testMathMinFunctionParameters()
    {
        $this->smarty->disableSecurity();
        $expected = min(1, 2) . ' -- ' . min(1, 2, 0);
        $tpl = $this->smarty->createTemplate('eval:{$x = 1}{$y = 2}{$z = 0}{math equation="min(x, y)" x=$x y=$y} -- {math equation="min(x, y, z)" x=$x y=$y z=$z}');
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

	public function testBackticksIllegal()
	{
		$this->expectException(PHPUnit\Framework\Error\Warning::class);
		$expected = "22.00";
		$tpl = $this->smarty->createTemplate('eval:{$x = "4"}{$y = "5.5"}{math equation="`ls` x * y" x=$x y=$y}');
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	public function testDollarSignsIllegal()
	{
		$this->expectException(PHPUnit\Framework\Error\Warning::class);
		$expected = "22.00";
		$tpl = $this->smarty->createTemplate('eval:{$x = "4"}{$y = "5.5"}{math equation="$" x=$x y=$y}');
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	public function testBracketsIllegal()
	{
		$this->expectException(PHPUnit\Framework\Error\Warning::class);
		$expected = "I";
		$tpl = $this->smarty->createTemplate('eval:{$x = "0"}{$y = "1"}{math equation="((y/x).(x))[x]" x=$x y=$y}');
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	public function testRand()
	{
		$tpl = $this->smarty->createTemplate('eval:{$x = "0"}{math equation="x * rand()" x=$x}');
		// this assertion may seem silly, but it serves to prove that using rand() without a parameter
		// will not trigger a security error (see https://github.com/smarty-php/smarty/issues/794)
		$this->assertEquals("0", $this->smarty->fetch($tpl));
	}

}
