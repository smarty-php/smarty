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
      * @run InSeparateProcess
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
     * @run InSeparateProcess
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
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider functionProvider
     * test simple function call tag plugin
     *
     */
    public function testSimpleFunctionPlugin_003()
    {
        $this->smarty->assign('param', 1);
        $this->smarty->assign('default', 2, true);
        $this->assertEquals("default 1", $this->smarty->fetch('test_template_function_003.tpl'));
    }

    /**
     * @run InSeparateProcess
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
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider functionProvider
     * test simple function call recursive
     */
    public function testRecursiveFunction($text)
    {
        $this->assertEquals("012345", $this->smarty->fetch('test_template_function_tag4.tpl'), $text);
    }

    /**
     * @run InSeparateProcess
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
     * @run InSeparateProcess
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
     * @run InSeparateProcess
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
     * @run InSeparateProcess
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
     * Function data provider inline
     */
    public function functionProvider()
    {
        return array(
            array('compile'),
            array('call'),
        );
    }
}
