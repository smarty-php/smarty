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
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test For
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestFor
     * @runInSeparateProcess
     */
    public function testFor($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "For_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
    }

    /*
      * Data provider für testFor
      */
    public function dataTestFor()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(
            array('{for $x=0;$x<10;$x++}{$x}{/for}', '0123456789', 'T1', $i++),
            array('{for $x=0; $x<10; $x++}{$x}{forelse}else{/for}', '0123456789', 'T2', $i++),
                     array('{for $x=10;$x<10;$x++}{$x}{forelse}else{/for}', 'else', 'T3', $i++),
                     array('{for $x=9;$x>=0;$x--}{$x}{forelse}else{/for}', '9876543210', 'T4', $i++),
                     array('{for $x=-1;$x>=0;$x--}{$x}{forelse}else{/for}', 'else', 'T5', $i++),
                     array('{for $x=0,$y=10;$x<$y;$x++}{$x}{forelse}else{/for}', '0123456789', 'T6', $i++),
                     array('{for $x=0;$x<10;$x=$x+2}{$x}{/for}', '02468', 'T7', $i++),
                     array('{for $x=0 to 8}{$x}{/for}', '012345678', 'T8', $i++),
                     array('{for $x=0 to 8 step=2}{$x}{/for}', '02468', 'T9', $i++),
                     array('{for $x=0 to 8 step=2}{if $x@first}{$x} {$x@total}{/if}{/for}', '0 5', 'T10', $i++),
                     array('{for $x=0 to 8 step=2}{if $x@last}{$x} {$x@iteration}{/if}{/for}', '8 5', 'T11', $i++),
                     array('{for $x=8 to 0 step=-2}{$x}{/for}', '86420', 'T12', $i++),
                     array('{for $x=8 to 0 step=2}{$x}{forelse}step error{/for}', 'step error', 'T13', $i++),
                     array('{for $x=8 to 0 step -1 max=3}{$x}{/for}', '876', 'T14', $i++),
        );
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
     * @runInSeparateProcess
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
     * @runInSeparateProcess
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
     * @runInSeparateProcess
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
    /**
     * Test spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('buh', 'buh');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "Spacing - {$file}");
    }

    /*
      * Data provider für testSpacing
      */
    public function dataTestSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array("A{for \$bar = 1 to 2}{\$bar}{/for}C", "A12C", 'T1', $i++),
                     array("A{for \$bar = 1 to 2}\n{\$bar}{/for}C", "A12C", 'T2', $i++),
                     array("A{for \$bar = 1 to 2}{\$bar}\n{/for}C", "A1\n2\nC", 'T3', $i++),
                     array("A{for \$bar = 1 to 2}\n{\$bar}\n{/for}C", "A1\n2\nC", 'T4', $i++),
                     array("A\n{for \$bar = 1 to 2}{\$bar}{/for}C", "A\n12C", 'T5', $i++),
                     array("A{for \$bar = 1 to 2}{\$bar}{/for}\nC", "A12C", 'T6', $i++),
                     array("A{for \$bar = 1 to 2}{\$bar}{forelse}D{/for}C", "A12C", 'T7', $i++),
                     array("A{for \$bar = 1 to 2}{\$bar}\n{forelse}D{/for}C", "A1\n2\nC", 'T8', $i++),
                     array("{for \$x=-1;\$x>=0;\$x--}{\$x}{forelse}A{\$buh}B{/for}", "AbuhB", 'T9', $i++),
                     array("{for \$x=-1;\$x>=0;\$x--}{\$x}{forelse}\nA{\$buh}B{/for}", "AbuhB", 'T10', $i++),
                     array("{for \$x=-1;\$x>=0;\$x--}{\$x}{forelse}A{\$buh}\nB{/for}", "Abuh\nB", 'T11', $i++),
                     array("{for \$x=-1;\$x>=0;\$x--}{\$x}{forelse}\nA{\$buh}\nB{/for}", "Abuh\nB", 'T12', $i++),
                     array("{for \$x=-1;\$x>=0;\$x--}{\$x}{forelse}{\$buh}\nB{/for}", "buh\nB", 'T13', $i++),
                     array("{for \$x=-1;\$x>=0;\$x--}{\$x}{forelse}{\$buh}{/for}", "buh", 'T14', $i++),
        );
    }
}
