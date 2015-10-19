<?php
/**
 * Smarty PHPunit tests compilation of {for} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {for} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileForTest extends PHPUnit_Smarty
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
     * test {for $x=0;$x<10;$x++} tag
     */
    public function testFor1()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=0;$x<10;$x++}{$x}{/for}');
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testFor2()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=0; $x<10; $x++}{$x}{forelse}else{/for}');
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testFor3()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=10;$x<10;$x++}{$x}{forelse}else{/for}');
        $this->assertEquals("else", $this->smarty->fetch($tpl));
    }

    public function testFor4()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=9;$x>=0;$x--}{$x}{forelse}else{/for}');
        $this->assertEquals("9876543210", $this->smarty->fetch($tpl));
    }

    public function testFor5()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=-1;$x>=0;$x--}{$x}{forelse}else{/for}');
        $this->assertEquals("else", $this->smarty->fetch($tpl));
    }

    public function testFor6()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=0,$y=10;$x<$y;$x++}{$x}{forelse}else{/for}');
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    public function testFor7()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=0;$x<10;$x=$x+2}{$x}{/for}');
        $this->assertEquals("02468", $this->smarty->fetch($tpl));
    }

    public function testFor8()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=0 to 8}{$x}{/for}');
        $this->assertEquals("012345678", $this->smarty->fetch($tpl));
    }

    public function testFor9()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=0 to 8 step=2}{$x}{/for}');
        $this->assertEquals("02468", $this->smarty->fetch($tpl));
    }

    public function testFor10()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=0 to 8 step=2}{if $x@first}{$x} {$x@total}{/if}{/for}');
        $this->assertEquals("0 5", $this->smarty->fetch($tpl));
    }

    public function testFor11()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=0 to 8 step=2}{if $x@last}{$x} {$x@iteration}{/if}{/for}');
        $this->assertEquals("8 5", $this->smarty->fetch($tpl));
    }

    public function testFor12()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=8 to 0 step=-2}{$x}{/for}');
        $this->assertEquals("86420", $this->smarty->fetch($tpl));
    }

    public function testFor13()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=8 to 0 step=2}{$x}{forelse}step error{/for}');
        $this->assertEquals("step error", $this->smarty->fetch($tpl));
    }

    public function testFor14()
    {
        $tpl = $this->smarty->createTemplate('eval:{for $x=8 to 0 step -1 max=3}{$x}{/for}');
        $this->assertEquals("876", $this->smarty->fetch($tpl));
    }

    /*
    *  test for and nocache
    */
    public function testForNocacheVar1()
    {
        $this->smarty->caching = true;
        $tpl = $this->smarty->createTemplate('string:{for $x=$foo to 5}{$x} {/for}');
        $tpl->assign('foo', 1, true);
        $this->assertFalse($this->smarty->isCached($tpl));
        $this->assertEquals("1 2 3 4 5 ", $this->smarty->fetch($tpl));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForNocacheVar2()
    {
        $this->smarty->caching = true;
        $tpl = $this->smarty->createTemplate('string:{for $x=$foo to 5}{$x} {/for}');
        $tpl->assign('foo', 4, true);
        $this->assertTrue($this->smarty->isCached($tpl));
        $this->assertEquals("4 5 ", $this->smarty->fetch($tpl));
    }

    public function testForNocacheTag1()
    {
        $this->smarty->caching = true;
        $tpl = $this->smarty->createTemplate('string:{for $x=$foo to 5 nocache}{$x} {/for}');
        $tpl->assign('foo', 1);
        $this->assertFalse($this->smarty->isCached($tpl));
        $this->assertEquals("1 2 3 4 5 ", $this->smarty->fetch($tpl));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForNocacheTag2()
    {
        $this->smarty->caching = true;
        $tpl = $this->smarty->createTemplate('string:{for $x=$foo to 5 nocache}{$x} {/for}');
        $tpl->assign('foo', 4);
        $this->assertTrue($this->smarty->isCached($tpl));
        $this->assertEquals("4 5 ", $this->smarty->fetch($tpl));
    }

    public function testForCache1()
    {
        $this->smarty->caching = true;
        $tpl = $this->smarty->createTemplate('string:{for $x=$foo to 2}{$x} {/for}');
        $tpl->assign('foo', 1);
        $this->assertFalse($this->smarty->isCached($tpl));
        $this->assertEquals("1 2 ", $this->smarty->fetch($tpl));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForCache2()
    {
        $this->smarty->caching = true;
        $tpl = $this->smarty->createTemplate('string:{for $x=$foo to 2}{$x} {/for}');
        $tpl->assign('foo', 6);
        $this->assertTrue($this->smarty->isCached($tpl));
        $this->assertEquals("1 2 ", $this->smarty->fetch($tpl));
    }
}
