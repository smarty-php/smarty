<?php
/**
 * Smarty PHPunit tests compilation of the {include} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {include} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileIncludeTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test spacing
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProviderCaching
     */
    public function testSpacing_001($merge, $caching, $text)
    {
        $this->smarty->setCaching($caching);
        if ($merge) {
            $this->smarty->setCacheId('1');
        }
        $this->smarty->setMergeCompiledIncludes($merge);
        $content = $this->smarty->fetch('test_include_001.tpl');
        $this->assertEquals('I1I2I3', $content, $text);
    }

    /**
     * test spacing
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProviderCaching
     */
    public function testSpacing_001V2($merge, $caching, $text)
    {
        $this->smarty->setCaching($caching);
        if ($merge) {
            $this->smarty->setCacheId('1');
        }
        $this->smarty->setMergeCompiledIncludes($merge);
        $content = $this->smarty->fetch('test_include_001V2.tpl');
        $this->assertEquals('I1I2I3', $content, $text);
    }

    /**
     * test spacing
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProviderCaching
     */
    public function testSpacing_001V3($merge, $caching, $text)
    {
        $this->smarty->setCaching($caching);
        if ($merge) {
            $this->smarty->setCacheId('1');
        }
        $this->smarty->setMergeCompiledIncludes($merge);
        $content = $this->smarty->fetch('test_include_001V3.tpl');
        $this->assertEquals('I1I2I3', $content, $text);
    }

    /**
     * test standard output
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProvider
     */
    public function testIncludeStandard_001($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $tpl = $this->smarty->createTemplate('test_include_standard.tpl');
        $content = $this->smarty->fetch($tpl);
        $this->assertEquals("hello world", $content, $text);
    }

    /**
     * test standard output var
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProvider
     */
    public function testIncludeStandardNocacheVar($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes(false);
        $this->smarty->caching = true;
        $this->smarty->assign('foo', 'foo', true);
        $tpl = $this->smarty->createTemplate('test_include_standard_nocache_var.tpl', $this->smarty);
        $content = $this->smarty->fetch($tpl);
        $this->assertEquals("foo\n\nhello world", $content, $text);
    }

    /**
     * Test that assign attribute does not create standard output
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProvider
     */
    public function testIncludeAssign1($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $tpl = $this->smarty->createTemplate('test_include_assign1.tpl');
        $this->assertEquals("", $this->smarty->fetch($tpl), $text);
    }

    /**
     * Test that assign attribute does load variable
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProvider
     */
    public function testIncludeAssign2($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $tpl = $this->smarty->createTemplate('test_include_assign2.tpl');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl), $text);
    }

    /**
     * Test passing local vars eval
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProvider
     */
    public function testIncludePassVars($merge, $text)
    {
        //$this->smarty->caching = true;
        $this->smarty->setMergeCompiledIncludes($merge);
        $tpl = $this->smarty->createTemplate('test_include_pass_vars.tpl');
        $this->assertEquals("12", $this->smarty->fetch($tpl), $text);
    }

    /**
     * Test passing local vars include
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProvider
     */
    public function testIncludePassVars2($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $tpl = $this->smarty->createTemplate('test_include_pass_vars2.tpl');
        $this->assertEquals("12", $this->smarty->fetch($tpl), $text);
    }




    /**
     * Test  recursive includes
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProvider
     */
    public function testRecursiveIncludes1($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $this->smarty->assign('foo', 1);
        $this->smarty->assign('bar', 'bar');
        $content = $this->smarty->fetch('test_recursive_includes.tpl');
        $this->assertContains("before 1 bar<br>\nbefore 2 bar<br>\nbefore 3 bar<br>\nafter 3 bar<br>\nafter 2 bar<br>\nafter 1 bar<br>", $content, $text);
    }

    /**
     * Test  recursive includes 2
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider includeProvider
     */
    public function testRecursiveIncludes2($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $this->smarty->assign('foo', 1);
        $this->smarty->assign('bar', 'bar');
        $content = $this->smarty->fetch('test_recursive_includes2.tpl');
        $this->assertContains("before 1 bar<br>\nbefore 3 bar<br>\nbefore 5 bar<br>\nafter 5 bar<br>\nafter 3 bar<br>\nafter 1 bar<br>", $content, $text);
    }

    /**
     * Include data provider
     */
    public function includeProvider()
    {
        return array(
            array(false, 'normal'),
            array(true, 'merged'),
        );
    }

    /**
     * Include data provider caching
     *
     * @dataProvider fileProvider
     */
    public function includeProviderCaching($file)
    {
        return array(
            array(false, false, 'normal'),
            array(true, false, 'merged'),
            array(false, true, 'normal cached 1'),
            array(false, true, 'normal cached 2'),
            array(true, true, 'merged cached 1'),
            array(true, true, 'merged cached 2'),
        );
    }

    public function fileProvider()
    {
        return array(
            array('normal'),
            array('merged'),
        );
    }
    /**
     * Test Include spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testIncludeSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->addTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testSpacing - {$file}");
    }
    /**
     * Test Output spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testIncludeSpacingNocache($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->smarty->setCompileId('1');
        $this->smarty->setCaching(1);
        $this->smarty->addTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar',true);
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testSpacing - {$file}");
    }
    /**
     * Test Output spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testIncludeSpacingNocache2($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->smarty->setCompileId('1');
        $this->smarty->setCaching(1);
        $this->smarty->addTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'foo',true);
        $this->assertEquals(str_replace('bar','foo',$result),
                            $this->smarty->fetch($file),
                            "testSpacing - {$file}");
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
        return array(array('A{include file=\'include_spacing2.tpl\'}B', 'A barB', '2_Text1', $i++),
                     array('A {include file=\'include_spacing2.tpl\'}B', 'A  barB', '2_Text2', $i++),
                     array('A{include file=\'include_spacing2.tpl\'}B', 'A barB', '2_Text3', $i++),
                     array("A{include file='include_spacing2.tpl'}\nB", "A barB", '2_Newline1', $i++),
                     array("A\n{include file='include_spacing2.tpl'}\nB", "A\n barB", '2_Newline2', $i++),
                     array("A{include file='include_spacing2.tpl'}B\nC", "A barB\nC", '2_Newline3', $i++),
                     array('A{include file=\'include_spacing3.tpl\'}B', "AbarB", '3_Text1', $i++),
                     array('A {include file=\'include_spacing3.tpl\'}B', "A barB", '3_Text2', $i++),
                     array('A{include file=\'include_spacing3.tpl\'}B', "AbarB", '3_Text3', $i++),
                     array("A{include file='include_spacing3.tpl'}\nB", "AbarB", '3_Newline1', $i++),
                     array("A\n{include file='include_spacing3.tpl'}\nB", "A\nbarB", '3_Newline2', $i++),
                     array("A{include file='include_spacing3.tpl'}B\nC", "AbarB\nC", '3_Newline3', $i++),
        );
    }
}
