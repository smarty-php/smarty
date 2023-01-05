<?php
/**
 * Smarty PHPunit tests compilation of {foreach} tag
 *

 * @author  Uwe Tews
 */

/**
 * class for {foreach} tag tests
 *
 * 
 * @preserveGlobalState    disabled
 * 
 */
class CompileForeachTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test foreach tags
     *
     *
     * 
     * @dataProvider        dataTestForeach
     */
    public function testForeach($code, $foo, $result, $testName, $testNumber)
    {
        $file = "testForeach_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assign('x', 'x');
        $this->smarty->assign('y', 'y');
        if ($foo !== null) {
            $this->smarty->assign('foo', $foo);
        } else {
            // unassigned $from parameter
            $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE | E_WARNING));
        }

        $this->assertEquals($result, $this->smarty->fetch($file), "testForeach - {$code} - {$testName}");
    }

    /*
      * Data provider für testForeach
      */
    public function dataTestForeach()
    {
        $i = 0;
        /*
        * Code
        *  $foo value
        * result
        * test name
        */
        return array(
            array('{foreach item=x from=$foo}{$x}{/foreach}', array(1,2,3), '123', '', $i ++),
            array('{foreach $foo as $x}{$x}{/foreach}', array(1,2,3), '123', '', $i ++),
            array('{foreach item=x from=$foo}{if $x == 2}{break}{/if}{$x}{/foreach}', array(0,1,2,3,4), '01', '', $i ++),
            array('{foreach item=x from=$foo}{if $x == 2}{continue}{/if}{$x}{/foreach}', array(0,1,2,3,4), '0134', '', $i ++),
            array('{foreach item=x from=$foo}{$x}{foreachelse}else{/foreach}', array(1,2,3), '123', '', $i ++),
            array('{foreach item=x from=$foo}{$x}{foreachelse}else{/foreach}', array(), 'else', '', $i ++),
            array('{foreach item=x from=$foo}{$x}{foreachelse}else{/foreach}', null, 'else', '', $i ++),
            array('{foreach item=x key=y from=$foo}{$y}=>{$x},{foreachelse}else{/foreach}', array(1,2,3), '0=>1,1=>2,2=>3,', '', $i ++),
            array('{foreach $foo as $y => $x}{$y}=>{$x},{foreachelse}else{/foreach}', array(1,2,3), '0=>1,1=>2,2=>3,', '', $i ++),
            array('{foreach $foo as $y => $x}{$y}=>{$x},{/foreach}-{$x}-{$y}', array(1,2,3), '0=>1,1=>2,2=>3,-x-y', 'saved loop variables', $i ++),
            array('{foreach $foo as $y => $x}{$y}=>{$x},{foreachelse}else{/foreach}-{$x}-{$y}', array(1,2,3), '0=>1,1=>2,2=>3,-x-y', 'saved loop variables', $i ++),
            array('{foreach $foo as $y => $x}{$y}=>{$x},{foreachelse}else{/foreach}-{$x}-{$y}', array(), 'else-x-y', 'saved loop variables', $i ++),
            array('{foreach $foo as $x}{$x@key}=>{$x},{foreachelse}else{/foreach}', array(1,2,3), '0=>1,1=>2,2=>3,', '', $i ++),
            array('{foreach item=x name=foo from=$foo}{$x}{foreachelse}else{/foreach}total{$smarty.foreach.foo.total}', array(1,2,3), '123total3', '', $i ++),
            array('{foreach item=x from=$foo}{$x}{foreachelse}else{/foreach}total{$x@total}', array(1,2,3), '123total3', '', $i ++),
            array('{foreach item=x name=foo from=$foo}{$smarty.foreach.foo.index}.{$x},{/foreach}', array(9,10,11), '0.9,1.10,2.11,', '', $i ++),
            array('{foreach item=x from=$foo}{$x@index}.{$x},{/foreach}', array(9,10,11), '0.9,1.10,2.11,', '', $i ++),
            array('{foreach item=x name=foo from=$foo}{$smarty.foreach.foo.iteration}.{$x},{/foreach}', array(9,10,11), '1.9,2.10,3.11,', '', $i ++),
            array('{foreach item=x from=$foo}{$x@iteration}.{$x},{/foreach}', array(9,10,11), '1.9,2.10,3.11,', '', $i ++),
            array('{foreach item=x from=$foo}{$x@iteration}.{$x}-{$x=\'foo\'}{$x},{/foreach}', array(9,10,11), '1.9-foo,2.10-foo,3.11-foo,', '', $i ++),
            array('{foreach item=x name=foo from=$foo}{if $smarty.foreach.foo.first}first{/if}{$x},{/foreach}', array(9,10,11), 'first9,10,11,', '', $i ++),
            array('{foreach item=x from=$foo}{if $x@first}first{/if}{$x},{/foreach}', array(9,10,11), 'first9,10,11,', '', $i ++),
            array('{foreach item=x name=foo from=$foo}{if $smarty.foreach.foo.last}last{/if}{$x},{/foreach}', array(9,10,11), '9,10,last11,', '', $i ++),
            array('{foreach item=x name=foo from=$foo}{if $smarty.foreach.foo.last}last{/if}{$smarty.foreach.foo.iteration}.{$x},{/foreach}', array(9,10,11), '1.9,2.10,last3.11,', '', $i ++),
            array('{foreach item=x from=$foo}{if $x@last}last{/if}{$x},{/foreach}', array(9,10,11), '9,10,last11,', '', $i ++),
            array('{foreach item=x name=foo from=$foo}{$x}{foreachelse}else{/foreach}{if $smarty.foreach.foo.show}-show{else}-noshow{/if}', array(9,10,11), '91011-show', '', $i ++),
            array('{foreach item=x name=foo from=$foo}{$x}{foreachelse}else{/foreach}{if $smarty.foreach.foo.show}-show{else}-noshow{/if}', array(), 'else-noshow', '', $i ++),
            array('{foreach item=x from=$foo}{$x}{foreachelse}else{/foreach}{if $x@show}-show{else}-noshow{/if}', array(9,10,11), '91011-show', '', $i ++),
            array('{foreach item=x from=$foo}{$x}{foreachelse}else{/foreach}{if $x@show}-show{else}-noshow{/if}', array(), 'else-noshow', '', $i ++),
            array('{foreach $foo x y foo}{$y}.{$x},{foreachelse}else{/foreach}total{$smarty.foreach.foo.total}', array(9,10,11), '0.9,1.10,2.11,total3', '', $i ++),
            array('{$x = "hallo"}{$bar=[1,2,3]}{foreach $foo as $x}outer={$x@index}.{$x}#{foreach $bar as $x}inner={$x@index}.{$x}{/foreach}##{/foreach}###{$x}', array(9,10,11), 'outer=0.9#inner=0.1inner=1.2inner=2.3##outer=1.10#inner=0.1inner=1.2inner=2.3##outer=2.11#inner=0.1inner=1.2inner=2.3#####hallo', '', $i ++),
            array('{foreach $foo as $x}{$x}{foreachelse}else{/foreach}', null, 'else', '', $i ++),
            array('{foreach $foo as $x}{$x}{foreachelse}else{/foreach}', array(), 'else', '', $i ++),
            array('{foreach $foo as $x}{$x}{foreachelse}else{/foreach}', new \ArrayIterator(), 'else', '', $i ++),
        );
    }


    /**
     * Test foreach tags caching
     *
     *
     * 
     * @dataProvider        dataTestForeachNocache
     */
    public function testForeachCaching($code, $new, $assignNocache, $foo, $result, $testName, $testNumber)
    {
        $this->smarty->caching = true;
        $file = "testForeachNocache_{$testNumber}.tpl";
        if ($new) {
            $this->makeTemplateFile($file, $code);
        }
        if ($foo !== null) {
            $this->smarty->assign('foo', $foo, $assignNocache);
        } else {
            // unassigned $from parameter
            $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
        }

        $this->assertEquals($result, $this->smarty->fetch($file), "testForeach - {$code} - {$testName}");
    }

    /*
   * Data provider für testForeachNocache
   */
    public function dataTestForeachNocache()
    {
        $i = 0;
        /*
        * Code
        * new name new file
        * assign nocache
        *  $foo value
        * result
        * test name
        */
        return array(
            array('{foreach item=x from=$foo}{$x}{/foreach}', true, true, array(1, 2, 3), '123', '', $i),
            array('{foreach item=x from=$foo}{$x}{/foreach}', false, true, array(4, 5, 6), '456', '', $i ++),
            array('{foreach item=x from=$foo}{$x}{/foreach}', true, false, array(1, 2, 3), '123', '', $i),
            array('{foreach item=x from=$foo}{$x}{/foreach}', false, false, array(4, 5, 6), '123', '', $i ++),
            array('{nocache}{foreach item=x from=$foo}{$x}{/foreach}{/nocache}', true, false, array(1, 2, 3), '123', '', $i),
            array('{nocache}{foreach item=x from=$foo}{$x}{/foreach}{/nocache}', false, false, array(4, 5, 6), '456', '', $i ++),
        );
    }
    /*
    *  test foreach and nocache
     *
     * 
     * 

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
     * 
     * 
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
     * 
     * 
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
     * 
     * 
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
     * 
     * 
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
     * 
     * 
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
        $this->assertEquals("a1a2b1b2for20a1a2b1b2for21",
                            $this->smarty->fetch('030_foreach.tpl'));
    }

    public function testForeachBreak_31()
    {
         $this->assertEquals("a1a2for20a1a2for21",
                            $this->smarty->fetch('031_foreach.tpl'));
    }

    public function testForeachContinue_32()
    {
        $this->assertEquals("1245",
                            $this->smarty->fetch('032_foreach.tpl'));
    }

    public function testForeachContinue_33()
    {
        $this->assertEquals("a1a2a4a5b1b2b4b5for20a1a2a4a5b1b2b4b5for21",
                            $this->smarty->fetch('033_foreach.tpl'));
    }

    public function testForeachContinue_34()
    {
        $this->assertEquals("a1a2b1b2for20a1a2b1b2for21",
                            $this->smarty->fetch('034_foreach.tpl'));
    }

    public function testForeachContinue_35()
    {
        $this->assertEquals("a1a2a1a2",
                            $this->smarty->fetch('035_foreach.tpl'));
    }
    public function testForeachIsset_36()
    {
        $this->assertEquals("false",
                            $this->smarty->fetch('036_foreach.tpl'));
    }
    public function testForeachIsset_37()
    {
        $this->assertEquals("false",
                            $this->smarty->fetch('037_foreach.tpl'));
    }
    /**
     * Test spacings
     *
     * 
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', array(1,2));
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
        return array(array("A{foreach from=\$foo item='bar'}{\$bar}{/foreach}C", "A12C", 'Newline1', $i++),
                     array("A{foreach from=\$foo item='bar'}\n{\$bar}{/foreach}C", "A12C", 'Newline2', $i++),
                     array("A{foreach from=\$foo item='bar'}{\$bar}\n{/foreach}C", "A1\n2\nC", 'Newline3', $i++),
                     array("A{foreach from=\$foo item='bar'}\n{\$bar}\n{/foreach}C", "A1\n2\nC", 'Newline4', $i++),
                     array("A\n{foreach from=\$foo item='bar'}{\$bar}{/foreach}C", "A\n12C", 'Newline5', $i++),
                     array("A{foreach from=\$foo item='bar'}{\$bar}{/foreach}\nC", "A12C", 'Newline6', $i++),
                     array("A{foreach from=\$foo item='bar'}{\$bar}{foreachelse}D{/foreach}C", "A12C", 'Newline7', $i++),
                     array("A{foreach from=\$foo item='bar'}{\$bar}\n{foreachelse}D{/foreach}C", "A1\n2\nC", 'Newline8', $i++),
                     array("{foreach from=\$foo item='bar' name='buh'}{\$bar}{/foreach}A{\$smarty.foreach.buh.total}C", "12A2C", 'Newline9', $i++),
                     array("{foreach from=\$foo item='bar' name='buh'}{\$bar}{/foreach}A\n{\$smarty.foreach.buh.total}C", "12A\n2C", 'Newline10', $i++),
                     array("{foreach from=\$foo item='bar' name='buh'}{\$bar}{/foreach}A{\$smarty.foreach.buh.total}\nC", "12A2\nC", 'Newline11', $i++),
        );
    }
    /**
     * Test spacings
     *
     * 
     * @dataProvider        dataTestElseSpacing
     * 
     */
    public function testElseSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_Else_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', array());
        $this->smarty->assign('buh', 'buh');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "Spacing - {$file}");
    }

    /*
      * Data provider für testSpacing
      */
    public function dataTestElseSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(
            array("{foreach from=\$foo item='bar'}{\$bar}{foreachelse}A{\$buh}B{/foreach}", "AbuhB", 'Newline1', $i++),
            array("{foreach from=\$foo item='bar'}{\$bar}{foreachelse}\nA{\$buh}B{/foreach}", "AbuhB", 'Newline2', $i++),
            array("{foreach from=\$foo item='bar'}{\$bar}{foreachelse}A{\$buh}\nB{/foreach}", "Abuh\nB", 'Newline3', $i++),
            array("{foreach from=\$foo item='bar'}{\$bar}{foreachelse}\nA{\$buh}\nB{/foreach}", "Abuh\nB", 'Newline4', $i++),
            array("{foreach from=\$foo item='bar'}{\$bar}{foreachelse}{\$buh}\nB{/foreach}", "buh\nB", 'Newline5', $i++),
            array("{foreach from=\$foo item='bar'}{\$bar}{foreachelse}{\$buh}{/foreach}", "buh", 'Newline6', $i++),
           );
    }

}
