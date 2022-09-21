<?php
/**
 * Smarty PHPunit tests compilation of {if} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {if} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileIfTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("../../../__shared/templates/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test if tags
     *
     * @not                 runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestIf
     */
    public function testIf($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "testIf_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('bar', 'buh');
        $this->assertEquals($result, $this->smarty->fetch($file),
                            "testIf - {$code} - {$name}");
    }

    /*
      * Data provider für testIf
      */
    public function dataTestIf()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    */
        return array(array('{if 0<1}yes{/if}', 'yes', '', $i ++),
                     array('{if false}false{elseif 0<1}yes{/if}', 'yes', '', $i ++),
                     array('{if 2<1}yes{else}no{/if}', 'no', '', $i ++),
                     array('{if 2<1}yes{elseif 4<5}yes1{else}no{/if}', 'yes1', '', $i ++),
                     array('{if 2<1}yes{elseif 6<5}yes1{else}no{/if}', 'no', '', $i ++),
                     array('{if false}false{elseif true}yes{else}no{/if}', 'yes', '', $i ++),
                     array('{if false}yes{else}no{/if}', 'no', '', $i ++),
                     array('{if !(1<2)}yes{else}no{/if}', 'no', '', $i ++),
                     array('{if not (true)}yes{else}no{/if}', 'no', '', $i ++),
                     array('{if 1 == 1}yes{else}no{/if}', 'yes', '', $i ++),
                     array('{if 1 EQ 1}yes{else}no{/if}', 'yes', '', $i ++),
                     array('{if 1 eq 1}yes{else}no{/if}', 'yes', '', $i ++),
                     array('{$foo=true}{if $foo===true}yes{else}no{/if}', 'yes', '', $i ++),
                     array('{$foo=true}{if $foo!==true}yes{else}no{/if}', 'no', '', $i ++),
                     array('{if 1 > 0}yes{else}no{/if}', 'yes', '', $i ++),
                     array('{if $x=1}yes{else}no{/if}{$x}', 'yes1', '', $i ++),
                     array('{$x=0}{if $x++}yes{else}no{/if} {$x}', 'no 1', '', $i ++),
                     array('{$x=[1,2]}{if $x[] = 7}{$x|var_export:true}{else}no{/if}', var_export(array(0=>1,1=>2,2=>7,),true), '',
                           $i ++),
                     array('{$x=[1,2]}{if $x[][\'a\'] = 7}{$x|var_export:true}{else}no{/if}',
                                         var_export(array(0=>1,1=>2,2=>array('a'=>7,),),true), '', $i ++),
                     array('{$foo=\'foo\'}{$bar=\'bar\'}{if $bar = "new_{$foo|default:\'\'}"}yes-{else}no{/if}{$bar}',
                           'yes-new_foo', '', $i ++),
                     array('{$foo=\'foo\'}{$bar=\'bar\'}{if false}false{elseif $bar = "new_{$foo|default:\'\'}"}yes-{else}no{/if}{$bar}',
                           'yes-new_foo', '', $i ++),
                     array('{$foo=\'foo\'}{$bar=\'bar\'}{if false}false{elseif $bar[3] = "new_{$foo|default:\'\'}"}yes-{else}no{/if}{$bar[0]}-{$bar[3]}',
                           'yes-bar-new_foo', '', $i ++),

                     array('{$x=0}{if $x}yes{else}no{/if}', 'no', 'AssignVar', $i ++),
                     array('{$x=0}{if $x++}yes{else}no{/if} {$x}', 'no 1', 'IncVar', $i ++),
                     array('{$x=1}{if $x}yes{else}no{/if}', 'yes', 'SimpleVar', $i ++),
                     array('{if $x=true}yes{else}no{/if}', 'yes', 'AssignTrue', $i ++),
                     array('{if $x=false}yes{else}no{/if}', 'no', 'AssignFalse', $i ++),
                     array('{if 3 ge strlen("foo")}yes{else}no{/if}', 'yes', 'CmpWithFunc', $i ++),
                     array('{if isset($foo)}yes{else}no{/if}', 'no', 'NotIsset', $i ++),
                     array('{$foo=1}{if isset($foo)}yes{else}no{/if}', 'yes', 'Isset', $i ++),
                     array('{$foo=1}{if !isset($foo)}yes{else}no{/if}', 'no', 'IssetNegate', $i ++),
                     array('{$foo=\'\'}{if empty($foo)}yes{else}no{/if}', 'yes', 'Empty', $i ++),
                     array('{$foo=\'foo\'}{if empty($foo)}yes{else}no{/if}', 'no', 'NotEmpty', $i ++),
                     array('{if 6 is div by 3}yes{else}no{/if}', 'yes', 'IsDivBy', $i ++),
                     array('{if 6 is not div by 3}yes{else}no{/if}', 'no', 'IsNotDivBye', $i ++),
                     array('{if 6 is even}yes{else}no{/if}', 'yes', 'IsEven', $i ++),
                     array('{if 6 is not even}yes{else}no{/if}', 'no', 'IsNotEven', $i ++),
                     array('{if 3 is odd}yes{else}no{/if}', 'yes', 'IsOdd', $i ++),
                     array('{if 3 is not odd}yes{else}no{/if}', 'no', 'IsNotOdd', $i ++),
                     array('{$foo=3}{if 3 is odd by $foo}yes{else}no{/if}', 'yes', 'IsOddByVar', $i ++),
                     array('{$foo=3}{$bar=6}{if $bar is not odd by $foo}yes{else}no{/if}', 'yes', 'IsNotOddByVar', $i ++),
                     array('{$foo=3}{$bar=3}{if 3+$bar is not odd by $foo}yes{else}no{/if}', 'yes', 'ExprIsNotOddByVar', $i ++),
                     array('{$foo=2}{$bar=6}{if (3+$bar) is not odd by ($foo+1)}yes{else}no{/if}', 'no', 'ExprIsNotOddByExpr', $i ++),
                     array('{if strlen("hello world") ===  11}yes{else}no{/if}', 'yes', 'FuncCmp', $i ++),
                     array('{if 0>1}yes{else}no{/if}', 'no', 'GT2', $i ++),
                     array('{if 1 GT 0}yes{else}no{/if}', 'yes', 'GT3', $i ++),
                     array('{if 0 gt 1}yes{else}no{/if}', 'no', 'GT4', $i ++),
                     array('{if 1 >= 0}yes{else}no{/if}', 'yes', 'GE1', $i ++),
                     array('{if 1>=1}yes{else}no{/if}', 'yes', 'GE2', $i ++),
                     array('{if 1 GE 1}yes{else}no{/if}', 'yes', 'GE3', $i ++),
                     array('{if 0 ge 1}yes{else}no{/if}', 'no', 'GE4', $i ++),
                     array('{if 0 < 0}yes{else}no{/if}', 'no', 'LT1', $i ++),
                     array('{if 0<1}yes{else}no{/if}', 'yes', 'LT2', $i ++),
                     array('{if 0 <= 0}yes{else}no{/if}', 'yes', 'LE1', $i ++),
                     array('{if 0<=1}yes{else}no{/if}', 'yes', 'LE2', $i ++),
                     array('{if 1 LE 0}yes{else}no{/if}', 'no', 'LE3', $i ++),
                     array('{if 0 le 1}yes{else}no{/if}', 'yes', 'LE4', $i ++),
                     array('{if 1 != 1}yes{else}no{/if}', 'no', 'NE1', $i ++),
                     array('{if 1!=2}yes{else}no{/if}', 'yes', 'NE2', $i ++),
                     array('{if 1 NE 1}yes{else}no{/if}', 'no', 'NE3', $i ++),
                     array('{if 1 ne 2}yes{else}no{/if}', 'yes', 'NE4', $i ++),
                     array('{if 1 === "1"}yes{else}no{/if}', 'no', 'Ident1', $i ++),
                     array('{if "1" === "1"}yes{else}no{/if}', 'yes', 'Ident2', $i ++),
                     array('{if 1 > 0 && 5 < 6}yes{else}no{/if}', 'yes', 'And1', $i ++),
                     array('{if 1 > 0&&5 < 6}yes{else}no{/if}', 'yes', 'And2', $i ++),
                     array('{if 1 > 0 AND 5 > 6}yes{else}no{/if}', 'no', 'And3', $i ++),
                     array('{if (1 > 0) and (5 < 6)}yes{else}no{/if}', 'yes', 'And4', $i ++),
                     array('{if 1 > 0 || 7 < 6}yes{else}no{/if}', 'yes', 'Or1', $i ++),
                     array('{if 1 > 0||5 < 6}yes{else}no{/if}', 'yes', 'Or2', $i ++),
                     array('{if 1 > 0 OR 5 > 6}yes{else}no{/if}', 'yes', 'Or3', $i ++),
                     array('{if (0 > 0) or (9 < 6)}yes{else}no{/if}', 'no', 'Or4', $i ++),
                     array('{if ((7>8)||(1 > 0)) and (5 < 6)}yes{else}no{/if}', 'yes', 'AndOr1', $i ++),
                     array('{if {counter start=1} == 1}yes{else}no{/if}', 'yes', 'Tag1', $i ++),
                     array('{if false}false{elseif {counter start=1} == 1}yes{else}no{/if}', 'yes', 'Tag2', $i ++),
                     array('{if {counter start=1} == 0}false{elseif {counter} == 2}yes{else}no{/if}', 'yes', 'Tag3', $i ++),
         );
    }




    /**
     * Test if nocache tags
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestIfNocache
     */
    public function testIfNocache($var, $value, $code, $result, $testName, $testNumber, $file = null)
    {
        if (!isset($file)) {
            $file = "testIfNoCache_{$testNumber}.tpl";
        }
        if ($code) {
            $this->makeTemplateFile($file, $code);
        }
        $this->smarty->setCaching(true);
        $this->smarty->assign('file', $file, true);
        $this->smarty->assign($var, $value, true);
        $this->smarty->assign($var . '2', $value);
        $this->assertEquals($result, $this->strip($this->smarty->fetch('run_code_caching.tpl')),
                            "testIfNocahe - {$code} - {$testName}");
    }

    /*
      * Data provider für testIfNocache
      */
    public function dataTestIfNocache()
    {
        $i = 1;
        /*
         * var
         * value
                    * Code
                    * result
                    * test name
                    */
        return array(array('foo', true, '{if $foo}yes{else}no{/if}', 'yes', '', $i ++, 'testIfNoCache_Var1.tpl'),
                     array('foo', true, false, 'yes', '', $i ++, 'testIfNoCache_Var1.tpl'),
                     array('foo', false, false, 'no', '', $i ++, 'testIfNoCache_Var1.tpl'),
                     array('foo', false, false, 'no', '', $i ++, 'testIfNoCache_Var1.tpl'),
                     array('foo', true, '{$bar=$foo}{if $bar}yes{else}no{/if}', 'yes', '', $i ++,
                           'testIfNoCache_Var2.tpl'),
                     array('foo', true, false, 'yes', '', $i ++, 'testIfNoCache_Var2.tpl'),
                     array('foo', false, false, 'no', '', $i ++, 'testIfNoCache_Var2.tpl'),
                     array('foo', false, false, 'no', '', $i ++, 'testIfNoCache_Var2.tpl'),
                     array('foo', 1, '{if $bar=$foo}yes{else}no{/if}{$bar}', 'yes1', '', $i ++,
                           'testIfNoCache_Var3.tpl'),
                     array('foo', 1, false, 'yes1', '', $i ++, 'testIfNoCache_Var3.tpl'),
                     array('foo', 0, false, 'no0', '', $i ++, 'testIfNoCache_Var3.tpl'),
                     array('foo', 0, false, 'no0', '', $i ++, 'testIfNoCache_Var3.tpl'),
                     array('bar', 4, '{if $bar2=$bar+3}yes{else}no{/if}{$bar2}', 'yes7', '', $i ++,
                           'testIfNoCache_Var4.tpl'),
                     array('bar', 4, false, 'yes7', '', $i ++, 'testIfNoCache_Var4.tpl'),
                     array('bar', 0, false, 'yes3', '', $i ++, 'testIfNoCache_Var4.tpl'),
                     array('bar', 0, false, 'yes3', '', $i ++, 'testIfNoCache_Var4.tpl'),);
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
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
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
        return array(array("A{if false}false{elseif true}{\$bar}{/if}C", "AbarC", 'T1', $i++),
                     array("A{if false}false{elseif true}\n{\$bar}{/if}C", "AbarC", 'T2', $i++),
                     array("A{if false}false{elseif true}{\$bar}\n{/if}C", "Abar\nC", 'T3', $i++),
                     array("A{if false}false{elseif true}\n{\$bar}\n{/if}C", "Abar\nC", 'T4', $i++),
                     array("A\n{if false}false{elseif true}{\$bar}{/if}C", "A\nbarC", 'T5', $i++),
                     array("A{if false}false{elseif true}{\$bar}{/if}\nC", "AbarC", 'T6', $i++),
                     array("A{if false}false{elseif true}{\$bar}{else}D{/if}C", "AbarC", 'T7', $i++),
                     array("A{if false}false{elseif true}{\$bar}\n{else}D{/if}C", "Abar\nC", 'T8', $i++),
                     array("{if false}false{else}A{\$bar}B{/if}", "AbarB", 'T9', $i++),
                     array("{if false}false{else}\nA{\$bar}B{/if}", "AbarB", 'T10', $i++),
                     array("{if false}false{else}A{\$bar}\nB{/if}", "Abar\nB", 'T11', $i++),
                     array("{if false}false{else}\nA{\$bar}\nB{/if}", "Abar\nB", 'T12', $i++),
                     array("{if false}false{else}{\$bar}\nB{/if}", "bar\nB", 'T13', $i++),
                     array("{if false}false{else}{\$bar}{/if}", "bar", 'T14', $i++),
                     array("A{if false}false{elseif true}{\$bar}{/if}C", "AbarC", 'T15', $i++),
                     array("A{if false}false{elseif true}\n{\$bar}{/if}C", "AbarC", 'T16', $i++),
                     array("A{if false}false{elseif true}{\$bar}\n{/if}C", "Abar\nC", 'T17', $i++),
                     array("A{if false}false{elseif true}\n{\$bar}\n{/if}C", "Abar\nC", 'T18', $i++),
                     array("A\n{if false}false{elseif true}{\$bar}{/if}C", "A\nbarC", 'T19', $i++),
                     array("A{if false}false{elseif true}{\$bar}{/if}\nC", "AbarC", 'T20', $i++),
                     array("A{if false}false{elseif true}{\$bar}{else}D{/if}C", "AbarC", 'T21', $i++),
                     array("A{if false}false{elseif true}{\$bar}\n{else}D{/if}C", "Abar\nC", 'T22', $i++),
        );
    }


 }
