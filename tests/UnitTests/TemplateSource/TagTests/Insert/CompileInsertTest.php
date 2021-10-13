<?php
/**
 * Smarty PHPunit tests compilation of the {insert} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {insert} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileInsertTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir(dirname(__FILE__) . "/PHPunitplugins/");
        $this->smarty->enableSecurity();
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * Test For
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestInsert
     * @runInSeparateProcess
     */
    public function testInsert($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Insert_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assign('variable', 'test');
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
    }

    /*
      * Data provider für testInsert
      */
    public function dataTestInsert()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(
            array('start {insert name=\'test\' foo=\'bar\'} end', 'start insert function parameter value bar end', 'T1', $i++),
            array('start {insert name="test" foo=\'bar\'} end', 'start insert function parameter value bar end', 'T2', $i++),
            array('start {insert name=$variable  foo=\'bar\'} end', 'start insert function parameter value bar end', 'T3', $i++),
            array("start {insert name='test' foo='bar' assign=blar} end {\$blar}", 'start  end insert function parameter value bar', 'T4', $i++),
            array("start {insert name='test' foo='bar' assign=blar} end", 'start  end', 'T5', $i++),
        );
    }




    /**
     * test insert plugin
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testInsertPlugin1()
    {
        global $insertglobal;
        $insertglobal = 'global';
        $tpl = $this->smarty->createTemplate('insertplugintest.tpl');
        $tpl->assign('foo', 'bar');
        $this->assertEquals('param foo bar globalvar global', $this->smarty->fetch($tpl));
    }
    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * test insert plugin
     */
    public function testInsertPlugin2()
    {
        global $insertglobal;
        $insertglobal = 'global 2';
        $tpl = $this->smarty->createTemplate('insertplugintest.tpl');
        $tpl->assign('foo', 'buh');
        $this->assertEquals('param foo buh globalvar global 2', $this->smarty->fetch($tpl));
    }

    /**
     * test insert plugin caching
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testInsertPluginCaching1_1()
    {
        global $insertglobal;
        $insertglobal = 'global';
        $this->smarty->caching = true;
        $tpl = $this->smarty->createTemplate('insertplugintest.tpl');
        $tpl->assign('foo', 'bar', true);
        $this->assertEquals('param foo bar globalvar global', $this->smarty->fetch($tpl));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testInsertPluginCaching1_2()
    {
        $this->smarty->addPluginsDir(dirname(__FILE__) . "/PHPunitplugins/");
        global $insertglobal;
        $insertglobal = 'changed global 2';
        $this->smarty->caching = 1;
        $tpl = $this->smarty->createTemplate('insertplugintest.tpl');
        $tpl->assign('foo', 'buh', true);
//        $this->assertTrue($tpl->isCached());
        $this->assertEquals('param foo buh globalvar changed global 2', $this->smarty->fetch($tpl));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testInsertPluginCaching1_3()
    {
        $this->smarty->addPluginsDir(dirname(__FILE__) . "/PHPunitplugins/");
        global $insertglobal;
        $insertglobal = 'changed global';
        $this->smarty->caching = 1;
//        $this->smarty->setForceCompile(true);
        $this->smarty->assign('foo', 'bar', true);
        $this->assertEquals('param foo bar globalvar changed global', $this->smarty->fetch('insertplugintest.tpl'));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testInsertPluginCaching1_4()
    {
        global $insertglobal;
        $this->smarty->addPluginsDir(dirname(__FILE__) . "/PHPunitplugins/");
            $insertglobal = 'changed global 4';
            $this->smarty->caching = 1;
            $this->smarty->assign('foo', 'buh', true);
            $this->assertTrue($this->smarty->isCached('insertplugintest.tpl'));
            $this->assertEquals('param foo buh globalvar changed global 4', $this->smarty->fetch('insertplugintest.tpl'));
    }
    /**
     * test insert plugin caching 2
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testInsertPluginCaching2_1()
    {
        global $insertglobal;
        $insertglobal = 'global';
        $this->smarty->caching = true;
        $this->smarty->compile_id = 1;
        $tpl = $this->smarty->createTemplate('insertplugintest.tpl');
        $tpl->assign('foo', 'bar');
        $this->assertEquals('param foo bar globalvar global', $this->smarty->fetch($tpl));
    }

    /**
     * test insert plugin caching 2
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testInsertPluginCaching2_2()
    {
        global $insertglobal;
        $insertglobal = 'global 2';
        $this->smarty->caching = true;
        $this->smarty->compile_id = 1;
        $tpl = $this->smarty->createTemplate('insertplugintest.tpl');
        $tpl->assign('foo', 'buh');
        $this->assertStringContainsString('param foo bar globalvar global 2', $this->smarty->fetch($tpl));
    }
    /**
     * test insert plugin caching 3
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testInsertPluginCaching3_1()
    {
        $this->smarty->caching = true;
        $this->smarty->assign('insert',$t=time());
        $this->assertStringContainsString($t.'Inner template', $this->smarty->fetch('insertplugintest2.tpl'));
    }

    /**
     * test insert plugin caching 2
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @group slow
     */
    public function testInsertPluginCaching3_2()
    {
        sleep(2);
        $this->smarty->caching = true;
        $this->smarty->assign('insert',$t=time());
        $this->assertStringContainsString($t.'Inner template', $this->smarty->fetch('insertplugintest2.tpl'));
    }


    /**
     * test inserted function none existing function
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testInsertFunctionNoneExistingFunction()
    {
        $tpl = $this->smarty->createTemplate("eval:start {insert name='mustfail' foo='bar' assign=blar} end {\$blar}");
        try {
            $this->smarty->fetch($tpl);
        }
        catch (Exception $e) {
            $this->assertStringContainsString("{insert} no function or plugin found for 'mustfail'", $e->getMessage());

            return;
        }
        $this->fail('Exception for "function is not callable" has not been raised.');
    }

    /**
     * test inserted function none existing script
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testInsertFunctionNoneExistingScript()
    {
        $tpl = $this->smarty->createTemplate("eval:{insert name='mustfail' foo='bar' script='nofile.php'}");
        try {
            $this->smarty->fetch($tpl);
        }
        catch (Exception $e) {
            $this->assertStringContainsString('missing script file', $e->getMessage());

            return;
        }
        $this->fail('Exception for "missing file" has not been raised.');
    }
}

/**
 * test function
 */
function insert_test($params, $template)
{
    return "insert function parameter value $params[foo]";
}
