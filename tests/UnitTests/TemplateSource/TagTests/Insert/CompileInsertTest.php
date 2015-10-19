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
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addPluginsDir(__DIR__ . "/PHPunitplugins/");
        $this->smarty->enableSecurity();
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test inserted function
     */
    public function testInsertFunctionSingle()
    {
        $tpl = $this->smarty->createTemplate("eval:start {insert name='test' foo='bar'} end");
        $this->assertEquals("start insert function parameter value bar end", $this->smarty->fetch($tpl));
    }

    public function testInsertFunctionDouble()
    {
        $tpl = $this->smarty->createTemplate("eval:start {insert name=\"test\" foo='bar'} end");
        $this->assertEquals("start insert function parameter value bar end", $this->smarty->fetch($tpl));
    }

    public function testInsertFunctionVariableName()
    {
        $tpl = $this->smarty->createTemplate("eval:start {insert name=\$variable foo='bar'} end");
        $tpl->assign('variable', 'test');
        $this->assertEquals("start insert function parameter value bar end", $this->smarty->fetch($tpl));
    }

    /**
     * test insert plugin
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
     * @run InSeparateProcess
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
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testInsertPluginCaching1_2()
    {
        $this->smarty->addPluginsDir(__DIR__ . "/PHPunitplugins/");
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
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testInsertPluginCaching1_3()
    {
        $this->smarty->addPluginsDir(__DIR__ . "/PHPunitplugins/");
        global $insertglobal;
        $insertglobal = 'changed global';
        $this->smarty->caching = 1;
//        $this->smarty->setForceCompile(true);
        $this->smarty->assign('foo', 'bar', true);
        $this->assertEquals('param foo bar globalvar changed global', $this->smarty->fetch('insertplugintest.tpl'));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testInsertPluginCaching1_4()
    {
        global $insertglobal;
        $this->smarty->addPluginsDir(__DIR__ . "/PHPunitplugins/");
        if (true) {   //disabled
            $insertglobal = 'changed global 4';
            $this->smarty->caching = 1;
            $this->smarty->assign('foo', 'buh', true);
            $this->assertTrue($this->smarty->isCached('insertplugintest.tpl'));
            $this->assertEquals('param foo buh globalvar changed global 4', $this->smarty->fetch('insertplugintest.tpl'));
        }
    }
    /**
     * test insert plugin caching 2
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
     * @run InSeparateProcess
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
        $this->assertEquals('param foo bar globalvar global 2', $this->smarty->fetch($tpl));
    }

    /**
     * test inserted function with assign
     */
    public function testInsertFunctionAssign()
    {
        $tpl = $this->smarty->createTemplate("eval:start {insert name='test' foo='bar' assign=blar} end {\$blar}");
        $this->assertEquals("start  end insert function parameter value bar", $this->smarty->fetch($tpl));
    }

    /**
     * test insertfunction with assign no output
     */
    public function testInsertFunctionAssignNoOutput()
    {
        $tpl = $this->smarty->createTemplate("eval:start {insert name='test' foo='bar' assign=blar} end");
        $this->assertEquals("start  end", $this->smarty->fetch($tpl));
    }

    /**
     * test insert plugin with assign
     */
    public function testInsertPluginAssign()
    {
        global $insertglobal;
        $insertglobal = 'global';
        $tpl = $this->smarty->createTemplate("eval:start {insert name='insertplugintest' foo='bar' assign=blar} end {\$blar}");
        $tpl->assign('foo', 'bar');
        $this->assertEquals('start  end param foo bar globalvar global', $this->smarty->fetch($tpl));
    }

    /**
     * test inserted function none existing function
     */
    public function testInsertFunctionNoneExistingFunction()
    {
        $tpl = $this->smarty->createTemplate("eval:start {insert name='mustfail' foo='bar' assign=blar} end {\$blar}");
        try {
            $this->smarty->fetch($tpl);
        }
        catch (Exception $e) {
            $this->assertContains("{insert} no function or plugin found for 'mustfail'", $e->getMessage());

            return;
        }
        $this->fail('Exception for "function is not callable" has not been raised.');
    }

    /**
     * test inserted function none existing script
     */
    public function testInsertFunctionNoneExistingScript()
    {
        $tpl = $this->smarty->createTemplate("eval:{insert name='mustfail' foo='bar' script='nofile.php'}");
        try {
            $this->smarty->fetch($tpl);
        }
        catch (Exception $e) {
            $this->assertContains('missing script file', $e->getMessage());

            return;
        }
        $this->fail('Exception for "missing file" has not been raised.');
    }
}

/**
 * test function
 */
function insert_test($params)
{
    return "insert function parameter value $params[foo]";
}
