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
     * test standard output nocache var
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
     * Test scope
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestScope
     */
    public function testScope($code, $useSmarty, $result, $testName, $testNumber = null)
    {
        if ($testNumber) {
            $file = "testScope_{$testNumber}.tpl";
            $this->makeTemplateFile($file, $code);
            $this->smarty->assignGlobal('file', $file);
        }
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($useSmarty ? $this->smarty : null);
        $data->assign('foo', 'data');
        if (!$useSmarty) {
            $testName .= 'no smarty';
        }
        $this->assertEquals($this->strip($result), $this->strip($this->smarty->fetch('test_scope.tpl', $data)),
                            "test - {$code} - {$testName}");
    }

    /*
     * Data provider for testscope
     */
    public function dataTestScope()
    {
        $i = 1;
        return array(/*
             * Code
             * use Smarty object
             * result
             * test name
             */
                     array('{include \'test_scope_assign.tpl\'}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'data\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' bubble_up}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'data\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=parent}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'newvar\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=parent bubble_up}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'newvar\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=tpl_root}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'data\'#test_scope.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=tpl_root bubble_up}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'newvar\'#test_scope.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=root}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'data\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=root bubble_up}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'newvar\'#test_scope.tpl:$foo=\'newvar\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=root}', false, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'data\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=root bubble_up}', false, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'newvar\'#test_scope.tpl:$foo=\'newvar\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=smarty}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'data\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'newvar\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=smarty bubble_up}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'newvar\'#test_scope.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'newvar\'#global:$foo=\'global\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=global}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'data\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'newvar\'', '', $i++),
                     array('{include \'test_scope_assign.tpl\' scope=global bubble_up}', true, '#test_scope_assign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'newvar\'#test_scope.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'newvar\'', '', $i++),
                     array('{include \'test_scope_pluginassign.tpl\' scope=global}', true, '#test_scope_pluginassign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'data\'#test_scope.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'newvar\'', '', $i++),
                     array('{include \'test_scope_pluginassign.tpl\' scope=global bubble_up}', true, '#test_scope_pluginassign.tpl:$foo=\'newvar\'#testScope_'.$i.'.tpl:$foo=\'newvar\'#test_scope.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'newvar\'', '', $i++),
        );
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
}
