<?php
/**
 * Smarty PHPunit tests compilation of {function} tag
 *

 * @author  Uwe Tews
 */

use Smarty\CompilerException;

/**
 * class for {function} tag tests
 *
 *
 * 
 *
 */
class CompileFunctionTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
   }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * 
     * 
     * @dataProvider functionProvider
     * test simple function call tag
     */
    public function testSimpleFunction_001($text)
    {
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2);
        $this->assertEquals("default param default 1 2 1", $this->smarty->fetch('test_template_function_001.tpl'), $text);
    }
    /**
     * 
     * 
     * @dataProvider functionProvider
     * test simple function call tag
     *
     */
    public function testSimpleFunctionAssign_001($text)
    {
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2);
        $this->assertEquals("default param default 1 2 1", $this->smarty->fetch('test_template_function_assign_001.tpl'), $text);
    }

    /**
     * 
     * 
     * @dataProvider functionProvider
     * test simple function call tag cached
     */
    public function testSimpleFunctionCached_001($text)
    {
        $this->smarty->setCaching(1);
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2);
        $this->assertEquals("default param default 1 2 1", $this->smarty->fetch('test_template_function_001.tpl'), $text);
    }

     /**
      * 
      * 
      * @dataProvider functionProvider
     * test simple function call tag cached
     */
    public function testSimpleFunctionCached_002($text)
    {
        $this->smarty->setCaching(1);
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2);
        $this->assertEquals("default param default 1 2 1", $this->smarty->fetch('test_template_function_002.tpl'), $text);
    }


    /**
     * 
     * 
     * @dataProvider functionProvider
     * test simple function call tag cached no cache default variable
     */
    public function testSimpleFunctionCachedNocacheDefault_002_1($text)
    {
        $this->smarty->setCaching(1);
        $this->smarty->setCompileId(1);
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2, true);
        $this->assertEquals("default param default 1 2 1", $this->smarty->fetch('test_template_function_002.tpl'), $text);
    }

    /**
     * test simple function call tag cached no cache default variable 2
     *
     * 
     * 
     */
    public function testSimpleFunctionCachedNocacheDefault_002_2()
    {
        $this->smarty->setCaching(1);
        $this->smarty->setCompileId(1);
        $this->smarty->assign('param', 4);
        $this->smarty->assign('default', 8, true);
        $this->assertEquals("default param default 1 8 4", $this->smarty->fetch('test_template_function_002.tpl'));
    }

    /**
     * 
     * 
     * @dataProvider functionProviderCachingValue
     * test simple function call tag plugin
     *
     */
    public function testSimpleFunctionPlugin_003($caching, $text, $start,$result)
    {
        $this->smarty->setCaching($caching);
        $this->smarty->assign('start', $start, true);
        $this->smarty->assign('start1', $start+10);
        $this->assertEquals($result, $this->smarty->fetch('test_template_function_003.tpl'), $text);
    }




    /**
     * 
     * 
     * @dataProvider functionProvider
     * test simple function call tag 2
     *
     */
    public function testSimpleFunctionTag2($text)
    {
         $this->assertEquals("default param default param2 passed param2 default param", $this->smarty->fetch('test_template_function_tag2.tpl'), $text);
    }


    /**
     * 
     * 
     * @dataProvider functionProvider
     * test simple function call recursive
     */
    public function testRecursiveFunction($text)
    {
        $this->assertEquals("012345", $this->smarty->fetch('test_template_function_tag4.tpl'), $text);
    }

    /**
     * 
     * 
     * @dataProvider functionProviderInline
     * test inherited function call tag
     *
     */
    public function testInheritedFunction($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
         $this->assertEquals("012345", $this->smarty->fetch('test_template_function_tag5.tpl'), $text);
    }

    /**
     * 
     * 
     * @dataProvider functionProviderInline
     * test function definition in include
     *
      */
    public function testDefineFunctionInclude($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $tpl = $this->smarty->createTemplate('test_template_function_tag6.tpl');
        $this->assertEquals("012345", $this->smarty->fetch('test_template_function_tag6.tpl'), $text);
    }


    /**
     * 
     * 
     * @dataProvider functionProviderInline
     * test external function definition cached
     *
     */
    public function testExternalDefinedFunctionCached1($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $cacheId = $merge ? 'merge' : null;
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('test_template_function.tpl', $cacheId);
        $tpl->assign('foo', 'foo');
        $this->assertStringContainsString('foo foo', $this->smarty->fetch($tpl), $text);
    }

    /**
     * 
     * 
     * test external function definition and called by fetch
     *
     */
    public function testExternalDefinedFunctionCalledByFetch()
    {
        $this->smarty->assign('foo', 'foo');
        $this->assertStringContainsString('foo foo', $this->smarty->fetch('test_template_function.tpl'));
        $this->smarty->assign('foo', 'bar');
        $this->assertStringContainsString('bar bar', $this->smarty->fetch('test_template_function_call.tpl'));
    }

    /**
     * 
     * 
     * @dataProvider functionProviderInline
     * test external function definition cached 2
     *
     */
    public function testExternalDefinedFunctionCached12($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $cacheId = $merge ? 'merge' : null;
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('test_template_function.tpl', $cacheId);
        $this->assertTrue($this->smarty->isCached($tpl), $text);
        $tpl->assign('foo', 'bar');
        $this->assertStringContainsString('foo bar', $this->smarty->fetch($tpl), $text);
    }

    /**
     * 
     * 
     * @dataProvider functionProviderInline
     * test external function definition nocache call
     *
     */
    public function testExternalDefinedFunctionNocachedCall1($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $cacheId = $merge ? 'merge' : null;
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('test_template_function_nocache_call.tpl', $cacheId);
        $tpl->assign('foo', 'foo');
        $this->assertStringContainsString('foo foo', $this->smarty->fetch($tpl), $text);
    }

    /**
     * 
     * 
     * @dataProvider functionProviderInline
     * test external function definition nocache call 2
     *
     */
    public function testExternalDefinedFunctionNocachedCall2($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $cacheId = $merge ? 'merge' : null;
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('test_template_function_nocache_call.tpl', $cacheId);
        $this->assertTrue($this->smarty->isCached($tpl), $text);
        $tpl->assign('foo', 'bar');
        $this->assertStringContainsString('bar bar', $this->smarty->fetch($tpl), $text);
    }

    /**
     * test external function definition nocache call 3
     *
     * 
     * 
     * @dataProvider functionProviderInline
     */
    public function testExternalDefinedFunctionNocachedCall3($merge, $text)
    {
        $this->smarty->setMergeCompiledIncludes($merge);
        $cacheId = $merge ? 'merge' : null;
        $this->smarty->caching = 1;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('test_template_function_nocache_call.tpl', $cacheId);
        $this->assertTrue($this->smarty->isCached($tpl), $text);
        $tpl->assign('foo', 'bar');
        $this->assertStringContainsString('bar bar', $this->smarty->fetch($tpl), $text);
    }

    /**
     * test external defined recursion
     *
     * 
     * 
     * @dataProvider functionProvider
     */
    public function testExternalDefinedFunctionRecursion($text)
    {
        $this->assertEquals('012345', $this->smarty->fetch('test_template_function_recursion2.tpl'), $text);
    }

    /**
     * Function data provider inline
     */
    public function functionProviderInline()
    {
        return array(
            array(false, 'normal compile'),
            array(false, 'normal call'),
            array(true, 'merged compile'),
            array(true, 'merged call'),
        );
    }
    /**
     * Function data provider
     */
    public function functionProvider()
    {
        return array(
            array('compile'),
            array('call'),
        );
    }
    /**
     * Function data provider
     */
    public function functionProviderCaching()
    {
        return array(
            array(false, 'normal compile'),
            array(false, 'normal call'),
            array(true, 'cached compile'),
            array(true, 'cached call'),
        );
    }
    /**
     * Function data provider
     */
    public function functionProviderCachingValue()
    {
        return array(
            array(false, 'normal compile',5,'15 215 5'),
            array(false, 'normal call',3,'13 213 3'),
            array(true, 'cached compile',6,'16 216 6'),
            array(true, 'cached call',8,'16 218 8'),
        );
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
        $this->smarty->addTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
    }
    /**
     * Test Output nocache spacings
     *
     * 
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testSpacingNocache($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->smarty->setCompileId('VarNocache');
        $this->smarty->setCaching(1);
        $this->smarty->addTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar',true);
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testVarNocache - {$file}");
    }
    /**
     * Test Output nocache spacings
     *
     * 
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testSpacingNocache2($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->smarty->setCompileId('VarNocache');
        $this->smarty->setCaching(1);
        $this->smarty->addTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'foo',true);
        $this->assertEquals(str_replace('bar','foo',$result),
                            $this->smarty->fetch($file),
                            "testVarNocache1 - {$file}");
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
        return array(array("{include 'simple_function_lib.tpl'}A{call name='simple' bar=\$foo}C", "AbarC", 'T1', $i++),
                     array("{include 'simple_function_lib.tpl'}A\n{call name='simple' bar=\$foo}C", "A\nbarC", 'T2', $i++),
                     array("{include 'simple_function_lib.tpl'}A\n{call name='simple' bar=\$foo}\nC", "A\nbar\nC", 'T3', $i++),
                     array("{include 'simple_function_lib.tpl'}A\n{call name='simple' bar=\$foo}\nC", "A\nbar\nC", 'T4', $i++),
                     array("{include 'simple_function_lib.tpl'}A\n\n{call name='simple' bar=\$foo}\n\nC", "A\n\nbar\n\nC", 'T5', $i++),
                     array("{function name=simple}{\$bar}{/function}{call name='simple' bar=\$foo}", "bar", 'T6', $i++),
                     array("{function name=simple}A{\$bar}C{/function}{call name='simple' bar=\$foo}", "AbarC", 'T7', $i++),
                     array("{function name=simple}A\n{\$bar}C{/function}{call name='simple' bar=\$foo}", "A\nbarC", 'T8', $i++),
                     array("{function name=simple}A{\$bar}\nC{/function}{call name='simple' bar=\$foo}", "Abar\nC", 'T9', $i++),
                     array("{function name=simple}A\n{\$bar}\nC{/function}{call name='simple' bar=\$foo}", "A\nbar\nC", 'T10', $i++),
                     array("{function name=simple}{\$foo}{/function}{call name='simple'}", "bar", 'T11', $i++),
                     array("{function name=simple}A{\$foo}C{/function}{call name='simple'}", "AbarC", 'T12', $i++),
                     array("{function name=simple}A\n{\$foo}C{/function}{call name='simple'}", "A\nbarC", 'T13', $i++),
                     array("{function name=simple}A{\$foo}\nC{/function}{call name='simple'}", "Abar\nC", 'T14', $i++),
                     array("{function name=simple}A\n{\$foo}\nC{/function}{call name='simple'}", "A\nbar\nC", 'T15', $i++),
        );
    }

    /**
     * Test handling of function names that are a security risk
     */
    public function testIllegalFunctionName() {
        $this->expectException(CompilerException::class);
	    $this->smarty->fetch('string:{function name=\'rce(){};echo "hi";function \'}{/function}');
    }

}
