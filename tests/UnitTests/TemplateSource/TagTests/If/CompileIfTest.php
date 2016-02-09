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
    public function setUp()
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
        $file = "testIf_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('bar', 'buh');
        $this->assertEquals($this->strip($result), $this->strip($this->smarty->fetch($file)),
                            "testIf - {$code} - {$testName}");
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
                     array('{if true}yes{else}no{/if}', 'yes', '', $i ++),
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
                     array('{$x=0}{if $x++}yes{else}no{/if} {$x}', 'no1', '', $i ++),
                     array('{$x=[1,2]}{if $x[] = 7}{$x|var_export:true}{else}no{/if}', 'array(0=>1,1=>2,2=>7,)', '',
                           $i ++), array('{$x=[1,2]}{if $x[][\'a\'] = 7}{$x|var_export:true}{else}no{/if}',
                                         'array(0=>1,1=>2,2=>array(\'a\'=>7,),)', '', $i ++),
                     array('{$foo=\'foo\'}{$bar=\'bar\'}{if $bar = "new_{$foo|default:\'\'}"}yes-{else}no{/if}{$bar}',
                           'yes-new_foo', '', $i ++),
                     array('{$foo=\'foo\'}{$bar=\'bar\'}{if false}false{elseif $bar = "new_{$foo|default:\'\'}"}yes-{else}no{/if}{$bar}',
                           'yes-new_foo', '', $i ++),
                     array('{$foo=\'foo\'}{$bar=\'bar\'}{if false}false{elseif $bar[3] = "new_{$foo|default:\'\'}"}yes-{else}no{/if}{$bar[0]}-{$bar[3]}',
                           'yes-bar-new_foo', '', $i ++),);
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
        $this->assertEquals($this->strip($result), $this->strip($this->smarty->fetch('run_code_caching.tpl')),
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

    public function testIfGT2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0>1}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfGT3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 GT 0}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfGT4()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0 gt 1}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfGE1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 >= 0}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfGE2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1>=1}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfGE3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 GE 1}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfGE4()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0 ge 1}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfLT1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0 < 0}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfLT2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0<1}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfLT3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0 LT 1}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfLT4()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0 lt 1}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfLE1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0 <= 0}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfLE2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0<=1}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfLE3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 LE 0}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfLE4()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 0 le 1}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfNE1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 != 1}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfNE2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1!=2}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfNE3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 NE 1}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfNE4()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 ne 2}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfIdent1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 === "1"}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfIdent2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if "1" === "1"}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfAnd1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 > 0 && 5 < 6}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfAnd2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 > 0&&5 < 6}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfAnd3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 > 0 AND 5 > 6}}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfAnd4()
    {
        $tpl = $this->smarty->createTemplate('eval:{if (1 > 0) and (5 < 6)}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfOr1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 > 0 || 7 < 6}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfOr2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 > 0||5 < 6}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfOr3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 1 > 0 OR 5 > 6}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfOr4()
    {
        $tpl = $this->smarty->createTemplate('eval:{if (0 > 0) or (9 < 6)}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfAndOR4()
    {
        $tpl = $this->smarty->createTemplate('eval:{if ((7>8)||(1 > 0)) and (5 < 6)}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfIsDivBy()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 6 is div by 3}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfIsNotDivBy()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 6 is not div by 3}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfIsEven()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 6 is even}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfIsNotEven()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 6 is not even}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfIsOdd()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 3 is odd}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfIsNotOdd()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 3 is not odd}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfIsOddBy()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 3 is odd by 3}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfIsNotOddBy()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 6 is odd by 3}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfIsEvenBy()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 6 is even by 3}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfIsNotEvenBy()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 6 is not even by 3}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfFunc1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if strlen("hello world") ==  11}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfFunc2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if 3 ge strlen("foo")}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfFunc3()
    {
        $tpl = $this->smarty->createTemplate('eval:{if isset($foo)}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfFunc4()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=1}{if isset($foo)}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfStatement1()
    {
        $tpl = $this->smarty->createTemplate('eval:{if $x=true}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfStatement2()
    {
        $tpl = $this->smarty->createTemplate('eval:{if $x=false}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfVariable1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$x=1}{if $x}yes{else}no{/if}');
        $this->assertEquals("yes", $this->smarty->fetch($tpl));
    }

    public function testIfVariable2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$x=0}{if $x}yes{else}no{/if}');
        $this->assertEquals("no", $this->smarty->fetch($tpl));
    }

    public function testIfVariableInc1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$x=0}{if $x++}yes{else}no{/if} {$x}');
        $this->assertEquals("no 1", $this->smarty->fetch($tpl));
    }
}
