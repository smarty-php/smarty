<?php
/**
 * Smarty PHPunit tests compilation of block plugins
 *

 * @author  Uwe Tews
 */

/**
 * class for block plugin tests
 *
 *
 * @preserveGlobalState    disabled
 *
 */
class CompileBlockPluginTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $this->smarty->disableSecurity();
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test block plugin tag
     *
     */
    public function testBlockPluginNoAssign()
    {
        $this->assertEquals("hello world", $this->smarty->fetch('no_assign.tpl'));
    }

    /**
     * test block plugin tag with assign attribute
     *
     */
    public function testBlockPluginAssign()
    {
        $this->assertEquals("hello world", $this->smarty->fetch('assign.tpl'));
    }

    /**
     * test unknown block plugin tag
     */
    public function testBlockPluginUnknown()
    {
        $this->expectException(\Smarty\CompilerException::class);
        $this->expectExceptionMessage('unknown tag \'bar\'');
        $this->assertEquals("hello world", $this->smarty->fetch('unknown.tpl'));
    }

    /**
     * test block plugin function definition in script
     */
    public function testBlockPluginRegisteredFunction()
    {
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_BLOCK, 'blockplugintest', 'myblockplugintest');
        $this->assertEquals('block test', $this->smarty->fetch('registered.tpl'));
    }

    /**
     * test block plugin function definition in script
     */
    public function testBlockPluginRegisteredFunction2()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('block tag \'blockplugintest\' not callable');
        $this->assertEquals('block test', $this->smarty->fetch('registered.tpl'));
    }

    /**
     * test block plugin static method
     */
    public function testBlockPluginRegisteredStatic()
    {
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_BLOCK, 'blockpluginstatic', array('myblockclass1', 'staticfunc'));
        $this->assertEquals('static block test', $this->smarty->fetch('registered_static.tpl'));
    }

    /**
     * test block plugin static method failure
     */
    public function testBlockPluginRegisteredStatic2()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('block tag \'blockpluginstatic\' not callable');
        $this->assertEquals('static block test', $this->smarty->fetch('registered_static.tpl'));
    }

    /**
     * test block plugin object method
     */
    public function testBlockPluginRegisteredMethod()
    {
        $object = new myblockclass1();
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_BLOCK, 'blockpluginmethod', array($object, 'methodfunc'));
        $this->assertEquals('method block test', $this->smarty->fetch('registered_method.tpl'));
    }

    /**
     * test block plugin object method failure
     */
    public function testBlockPluginRegisteredMethod2()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('block tag \'blockpluginmethod\' not callable');
        $this->assertEquals('method block test', $this->smarty->fetch('registered_method.tpl'));
    }

    /**
     * test block plugin registered object
     */
    public function testBlockPluginRegisteredObject()
    {
        $object = new myblockclass1();
        $this->smarty->registerObject('myobject', $object, array(), true, array('objectfunc'));
        $this->assertEquals('object block test', $this->smarty->fetch('registered_object.tpl'));
    }

    /**
     * test block plugin registered object failure
     */
    public function testBlockPluginRegisteredObject2()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('block tag \'myobject\' not callable or registered');
        $this->assertEquals('object block test', $this->smarty->fetch('registered_object.tpl'));
    }

    /**
     * test block plugin repeat function
     */
    public function testBlockPluginRepeat()
    {
        $this->assertEquals('12345', $this->smarty->fetch('repeat.tpl'));
    }

    /**
     * test block plugin repeat function with modifier
     */
    public function testBlockPluginRepeatModidier1()
    {
        $this->assertEquals('11111', $this->smarty->fetch('repeat_modifier.tpl'));
    }

    /**
     * test block plugin repeat function with modifier list
     */
    public function testBlockPluginRepeatModidier2()
    {
        $this->assertEquals('11111', $this->smarty->fetch('repeat_modifier_2.tpl'));
    }

    /**
     * test block plugin with no output
     */
    public function testBlockPluginNoOutput()
    {
        $this->assertEquals('default', $this->smarty->fetch('nooutput.tpl'));
    }

    /**
     * test nested block plugin
     */
    public function testBlockPluginNested()
    {
        $this->assertEquals('hello world12345', $this->smarty->fetch('nested.tpl'));
    }

    /**
     * test default block plugin
     */
    public function testBlockPluginDefault1()
    {
        $this->smarty->registerDefaultPluginHandler('my_block_plugin_handler');
        $this->assertEquals('scriptblock hello world', $this->smarty->fetch('default1.tpl'));
    }

    /**
     * test default block plugin
     */
    public function testBlockPluginDefault2()
    {
        $this->smarty->registerDefaultPluginHandler('my_block_plugin_handler');
        $this->assertEquals('defaultblock hello world', $this->smarty->fetch('default2.tpl'));
    }

    /**
     * Test caching
     *
     * @dataProvider        data
     */
    public function testCache($isCached,
                              $caching,
                              $cachable,
                              $testNumber,
                              $compileTestNumber,
                              $renderTestNumber,
                              $resultNumber,
                              $nocache,
                              $compileid,
                              $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'prefilterTest'));
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_BLOCK, 'cachetest', 'myblockplugintest2', $cachable);
        $this->smarty->setCompileId($compileid);
        $this->smarty->caching = $caching;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('var', $testNumber, $nocache);
        $tpl = $this->smarty->createTemplate('caching.tpl', null, null, $this->smarty);
        if (isset($isCached)) {
            $this->assertEquals($isCached, $tpl->isCached(), $testName . ' - isCached()');
        }
        $result = $this->smarty->fetch($tpl);
        $this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}",
                              $result,
                              $testName . ' - fetch() failure test number');
        $this->assertStringContainsString("block test{$resultNumber}", $result, $testName . ' - fetch() failure result');
    }

    public function data()
    {
        return array(array(false, false, false, 1, 1, 1, 1, false, 0, 'no cacheing'),
                     array(false, false, false, 2, 1, 2, 2, false, 0, 'no cacheing'),
                     array(false, false, true, 3, 3, 3, 3, false, 1, 'cacheable'),
                     array(false, true, true, 4, 4, 4, 4, false, 1, 'cachable Caching'),
                     array(true, true, true, 5, 4, 4, 4, false, 1, 'cachable isCached'),
                     array(true, true, true, 6, 4, 4, 4, false, 1, 'cachable isCached'),
                     array(false, true, false, 7, 7, 7, 7, false, 2, 'not cachable'),
                     array(true, true, false, 8, 7, 7, 8, false, 2, 'not cachable isCached'),
                     array(true, true, false, 9, 7, 7, 9, false, 2, 'not cachable isCached'),
                     array(false, true, true, 10, 10, 10, 10, true, 3, 'not cachable nocache var'),
                     array(true, true, true, 11, 10, 10, 11, true, 3, 'not cachable isCached'),
                     array(true, true, true, 12, 10, 10, 12, true, 3, 'not cachable isCached'),);
    }

    /**
     * Test spacings
     *
     * @dataProvider        dataTestSpacing
     */
    public function testSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "testSpacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
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
        return array(array("A{noop}\nB{/noop}C", "ABC", 'Newline1', $i++),
                     array("A{noop}\nB\n{/noop}C", "AB\nC", 'Newline2', $i++),
                     array("A{noop}\nB{/noop}\nC", "ABC", 'Newline3', $i++),
                     array("A\n{noop}\nB\n{/noop}\nC", "A\nB\nC", 'Newline4', $i++),
                     array("A{noop}\n{\$foo}{/noop}C", "AbarC", 'Var1', $i++),
                     array("A{noop}\n{\$foo}\n{/noop}C", "Abar\nC", 'Var2', $i++),
                     array("A{noop}\n{\$foo}{/noop}\nC", "AbarC", 'Var3', $i++),
                     array("A\n{noop}\n{\$foo}\n{/noop}\nC", "A\nbar\nC", 'Var4', $i++),
        );
    }
    /**
     * Test spacings
     *
     * @dataProvider        dataTestDefaultSpacing
     */
    public function testSpacingDefault($code, $result, $testName, $testNumber)
    {
         $name = empty($testName) ? $testNumber : $testName;
        $file = "testSpacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->registerDefaultPluginHandler('my_block_plugin_handler');
        $this->smarty->setCompileId('default');
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testSpacing - {$file}");
    }

    /*
      * Data provider für testSpacing
      */
    public function dataTestDefaultSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array("A{scriptblock}\nB{/scriptblock}C", "Ascriptblock BC", 'Newline1', $i++),
                     array("A{scriptblock}\nB\n{/scriptblock}C", "Ascriptblock B\nC", 'Newline2', $i++),
                     array("A{scriptblock}\nB{/scriptblock}\nC", "Ascriptblock BC", 'Newline3', $i++),
                     array("A\n{scriptblock}\nB\n{/scriptblock}\nC", "A\nscriptblock B\nC", 'Newline4', $i++),
                     array("A{scriptblock}\n{\$foo}{/scriptblock}C", "Ascriptblock barC", 'Var1', $i++),
                     array("A{scriptblock}\n{\$foo}\n{/scriptblock}C", "Ascriptblock bar\nC", 'Var2', $i++),
                     array("A{scriptblock}\n{\$foo}{/scriptblock}\nC", "Ascriptblock barC", 'Var3', $i++),
                     array("A\n{scriptblock}\n{\$foo}\n{/scriptblock}\nC", "A\nscriptblock bar\nC", 'Var4', $i++),
        );
    }

    /**
     * Test nocache block spacings
     *
     * @dataProvider        dataTestNocacheSpacing
     */
    public function testBlockNocache($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Nocache_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setCompileId('nocache');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('bar', 'bar',true);
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testNocache - {$file}");
    }
    /**
     * Test nocache block spacings
     *
     * @dataProvider        dataTestNocacheSpacing
     */
    public function testBlockNocache2($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Nocache_{$name}.tpl";
        $this->smarty->setCompileId('nocache');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('bar', 'foo',true);
        $this->assertEquals(str_replace('bar','foo',$result),
                            $this->smarty->fetch($file),
                            "testNocache2 - {$file}");
    }

    /*
  * Data provider für testSpacing
  */
    public function dataTestNocacheSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array("A{testparameter value=\$bar}\n{\$foo}{/testparameter}C", "AbarC", 'Var1', $i++),
                     array("A{testparameter value=\$bar}\n{\$foo}\n{/testparameter}C", "Abar\nC", 'Var2', $i++),
                     array("A{testparameter value=\$bar}\n{\$foo}{/testparameter}\nC", "AbarC", 'Var3', $i++),
                     array("A\n{testparameter value=\$bar}\n{\$foo}\n{/testparameter}\nC", "A\nbar\nC", 'Var4', $i++),
    );
    }

}
function myblockplugintest($params, $content, $smarty_tpl, &$repeat)
{
    if (!$repeat) {
        $output = str_replace('hello world', 'block test', $content);

        return $output;
    }
}

function myblockplugintest2($params, $content, $smarty_tpl, &$repeat)
{
    if (!$repeat) {
        $output = str_replace('hello world', "block test{$params['var']}", $content);

        return $output;
    }
}

class myblockclass1
{
    static function staticfunc($params, $content, $smarty_tpl, &$repeat)
    {
        if (!$repeat) {
            $output = str_replace('hello world', 'static block test', $content);
            return $output;
        }
    }

    public function methodfunc($params, $content, $smarty_tpl, &$repeat)
    {
        if (!$repeat) {
            $output = str_replace('hello world', 'method block test', $content);
            return $output;
        }
    }

    public function objectfunc($params, $content, $smarty_tpl, &$repeat)
    {
        if (!$repeat) {
            $output = str_replace('hello world', 'object block test', $content);
            return $output;
        }
    }
}

function my_block_plugin_handler($tag, $type, $template, &$callback, &$script, &$cachable)
{
    switch ($type) {
        case \Smarty\Smarty::PLUGIN_BLOCK:
            switch ($tag) {
                case 'scriptblock':
                    $script = './scripts/script_block_tag2.php';
                    $callback = 'default_script_block_tag2';

                    return true;
                default:
                    $script = './scripts/other_block_tag.php';
                    $callback = 'default_block_tag';
                    return true;
            }
        default:
            return false;
    }
}

