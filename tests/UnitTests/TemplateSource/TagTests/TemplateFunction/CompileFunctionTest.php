<?php
/**
 * Smarty PHPunit tests compilation of {function} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {function} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileFunctionTest extends PHPUnit_Smarty
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
      * @runInSeparateProcess
      * @preserveGlobalState disabled
      * @dataProvider functionProvider
     * test simple function call tag
      *
     */
    public function testSimpleFunction_001($text)
    {
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2);
        $this->assertEquals("default param default 1 2 1", $this->smarty->fetch('test_template_function_001.tpl'), $text);
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
      * @runInSeparateProcess
      * @preserveGlobalState disabled
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
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider functionProviderCaching
     * test simple function call tag plugin
     *
     */
    public function testSimpleFunctionPlugin_003($caching, $text)
    {
        $this->smarty->setCaching($caching);
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2, true);
        $this->assertEquals("default 1", $this->smarty->fetch('test_template_function_003.tpl'), $text);
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider functionProvider
     * test simple function call tag plugin nocache
     *
     */
    public function testSimpleFunctionCachedPluginNocache_003($text)
    {
        $this->smarty->setCaching(1);
        $this->smarty->setCompileId(1);
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2, true);
        $this->assertEquals("default 1", $this->smarty->fetch('test_template_function_003.tpl'), $text);
    }


    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider functionProvider
     * test simple function call tag 2
     *
     */
    public function testSimpleFunctionTag2($text)
    {
         $this->assertEquals("default param default param2 passed param2 default param", $this->smarty->fetch('test_template_function_tag2.tpl'), $text);
    }


    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider functionProvider
     * test simple function call recursive
     */
    public function testRecursiveFunction($text)
    {
        $this->assertEquals("012345", $this->smarty->fetch('test_template_function_tag4.tpl'), $text);
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
        $this->assertContains('foo foo', $this->smarty->fetch($tpl), $text);
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
        $this->assertContains('foo bar', $this->smarty->fetch($tpl), $text);
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
        $this->assertContains('foo foo', $this->smarty->fetch($tpl), $text);
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
        $this->assertContains('bar bar', $this->smarty->fetch($tpl), $text);
    }

    /**
     * test external function definition nocache call 3
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
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
        $this->assertContains('bar bar', $this->smarty->fetch($tpl), $text);
    }

    /**
     * test external defined recursion
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider functionProvider
     */
    public function testExternalDefinedFunctionRecursion($text)
    {
        $this->assertEquals('12345', $this->smarty->fetch('test_template_function_recursion2.tpl'), $text);
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
}
