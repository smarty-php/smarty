<?php
/**
 * Smarty PHPunit tests compilation of {foreach} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {foreach} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileForeachTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        //$this->smarty->force_compile = true;
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test {foreach} tag
     */
    public function testForeach_001()
    {
        $this->smarty->assign('foo', array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
        $this->assertEquals("0123456789", $this->smarty->fetch('001_foreach.tpl'));
    }

    public function testForeachBreak_002()
    {
        $this->smarty->assign('foo', array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
        $this->assertEquals("01", $this->smarty->fetch('002_foreach.tpl'));
    }

    public function testForeachContinue_003()
    {
        $this->smarty->assign('foo', array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
        $this->assertEquals("013456789", $this->smarty->fetch('003_foreach.tpl'));
    }

    public function testForeachNotElse_004()
    {
        $this->smarty->assign('foo', array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
        $this->assertEquals("0123456789", $this->smarty->fetch('004_foreach.tpl'));
    }

    public function testForeachElse_005()
    {
        $this->smarty->assign('foo', array());
        $this->assertEquals("else", $this->smarty->fetch('005_foreach.tpl'));
    }

    public function testForeachElse_005_2()
    {
        $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
        $this->assertEquals("else", $this->smarty->fetch('005_foreach.tpl'));
    }

    public function testForeachKey_006()
    {
        $this->assertEquals("09182736455463728190", $this->smarty->fetch('006_foreach.tpl'));
    }

    public function testForeachKeyProperty_007()
    {
        $this->assertEquals("09182736455463728190", $this->smarty->fetch('007_foreach.tpl'));
    }

    public function testForeachTotal_008()
    {
        $this->assertEquals("0123456789total10", $this->smarty->fetch('008_foreach.tpl'));
    }

    public function testForeachTotalProperty_009()
    {
        $this->assertEquals("0123456789total10", $this->smarty->fetch('009_foreach.tpl'));
    }

    public function testForeachIndexIteration_010()
    {
        $this->assertEquals("011223344556677889910", $this->smarty->fetch('010_foreach.tpl'));
    }

    public function testForeachIndexIterationProperty_011()
    {
        $this->assertEquals("011223344556677889910", $this->smarty->fetch('011_foreach.tpl'));
    }

    public function testForeachFirstLast_012()
    {
        $this->assertEquals("first012345678last9", $this->smarty->fetch('012_foreach.tpl'));
    }

    public function testForeachFirstLastProperty_013()
    {
        $this->assertEquals("first012345678last9", $this->smarty->fetch('013_foreach.tpl'));
    }

    public function testForeachShowTrue_014()
    {
        $this->assertEquals("01show", $this->smarty->fetch('014_foreach.tpl'));
    }

    public function testForeachShowTrueProperty_015()
    {
        $this->assertEquals("01show", $this->smarty->fetch('015_foreach.tpl'));
    }

    public function testForeachShowFalse_016()
    {
        $this->assertEquals("else noshow", $this->smarty->fetch('016_foreach.tpl'));
    }

    public function testForeachShowFalseProperty_017()
    {
        $this->assertEquals("else noshow", $this->smarty->fetch('017_foreach.tpl'));
    }

    public function testForeachShorttags_018()
    {
        $this->assertEquals("09182736455463728190total10", $this->smarty->fetch('018_foreach.tpl'));
    }

    /**
     * test {foreach $foo as $x} tag
     */
    public function testNewForeach_19()
    {
        $this->smarty->assign('foo', array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
        $this->assertEquals("0123456789", $this->smarty->fetch('019_foreach.tpl'));
    }

    public function testNewForeachNotElse_020()
    {
        $this->smarty->assign('foo', array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9));
        $this->assertEquals("0123456789", $this->smarty->fetch('020_foreach.tpl'));
    }

    public function testNewForeachElse_021()
    {
        $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
        $this->assertEquals("else", $this->smarty->fetch('021_foreach.tpl'));
    }

    public function testNewForeachElse_021_1()
    {
        $this->smarty->assign('foo', array());
        $this->assertEquals("else", $this->smarty->fetch('021_foreach.tpl'));
    }

    public function testNewForeachKey_022()
    {
        $this->smarty->assign('foo', array(9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
        $this->assertEquals("09182736455463728190", $this->smarty->fetch('022_foreach.tpl'));
    }

    public function testNewForeachKeyProperty_023()
    {
        $this->smarty->assign('foo', array(9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
        $this->assertEquals("09182736455463728190", $this->smarty->fetch('023_foreach.tpl'));
    }

    /*
    *  test foreach and nocache
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled

    */
    public function testForeachNocacheVar1_024()
    {
        $this->smarty->caching = true;
        $this->smarty->assign('foo', array(1, 2), true);
        $this->assertFalse($this->smarty->isCached('024_foreach.tpl'));
        $this->assertEquals("1 2 ", $this->smarty->fetch('024_foreach.tpl'));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForeachNocacheVar2_024()
    {
        $this->smarty->caching = true;
        $this->smarty->assign('foo', array(9, 8), true);
        $this->assertTrue($this->smarty->isCached('024_foreach.tpl'));
        $this->assertEquals("9 8 ", $this->smarty->fetch('024_foreach.tpl'));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForeachNocacheTag1_025()
    {
        $this->smarty->caching = true;
        $this->smarty->assign('foo', array(1, 2));
        $this->assertFalse($this->smarty->isCached('025_foreach.tpl'));
        $this->assertEquals("1 2 ", $this->smarty->fetch('025_foreach.tpl'));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForeachNocacheTag2_25()
    {
        $this->smarty->caching = true;
        $this->smarty->assign('foo', array(9, 8));
        $this->assertTrue($this->smarty->isCached('025_foreach.tpl'));
        $this->assertEquals("9 8 ", $this->smarty->fetch('025_foreach.tpl'));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForeachCache1_26()
    {
        $this->smarty->caching = true;
        $this->smarty->assign('foo', array(1, 2));
        $this->assertFalse($this->smarty->isCached('026_foreach.tpl'));
        $this->assertEquals("1 2 ", $this->smarty->fetch('026_foreach.tpl'));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForeachCache2_26()
    {
        $this->smarty->caching = true;
        $this->smarty->assign('foo', array(9, 8));
        $this->assertTrue($this->smarty->isCached('026_foreach.tpl'));
        $this->assertEquals("1 2 ", $this->smarty->fetch('026_foreach.tpl'));
    }

    public function testForeachNested_27()
    {
        $this->smarty->assign('foo', array(9, 8));
        $this->smarty->assign('bar', array(4, 10));
        $this->assertEquals("outer=0#9inner=0#4inner=1#10##outer=1#8inner=0#4inner=1#10#####hallo",
                            $this->smarty->fetch('027_foreach.tpl'));
    }

    public function testForeachNestedNamed_28()
    {
        $this->smarty->assign('foo', array(9, 8));
        $this->smarty->assign('bar', array(4, 10));
        $this->assertEquals("outer=0#0-9inner=1#0-4inner=2#0-10##outer=1#1-8inner=1#1-4inner=2#1-10#####hallo",
                            $this->smarty->fetch('028_foreach.tpl'));
    }

    public function testForeachBreak_29()
    {
        $this->assertEquals("12",
                            $this->smarty->fetch('029_foreach.tpl'));
    }

    public function testForeachBreak_30()
    {
        $this->assertEquals("a1a2b1b2",
                            $this->smarty->fetch('030_foreach.tpl'));
    }

    public function testForeachBreak_31()
    {
        $this->assertEquals("a1a2",
                            $this->smarty->fetch('031_foreach.tpl'));
    }

    public function testForeachContinue_32()
    {
        $this->assertEquals("1245",
                            $this->smarty->fetch('032_foreach.tpl'));
    }

    public function testForeachContinue_33()
    {
        $this->assertEquals("a1a2a4a5b1b2b4b5",
                            $this->smarty->fetch('033_foreach.tpl'));
    }

    public function testForeachContinue_34()
    {
        $this->assertEquals("a1a2b1b2",
                            $this->smarty->fetch('034_foreach.tpl'));
    }
}
