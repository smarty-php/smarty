<?php
/**
 * Smarty PHPunit tests compilation of assign tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for assign tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileAssignTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("./PHPunitplugins/");
        $this->smarty->registerFilter('pre', array($this, 'prefilterTest'));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test old style of assign tag
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignOld_001()
    {
        $this->assertEquals("1", $this->smarty->fetch('001_oldFormat_1.tpl'));
        $this->assertEquals("bar", $this->smarty->fetch('004_oldFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('005_oldFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('006_oldFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('007_oldFormat_1.tpl'));
        $this->assertEquals("9876", $this->smarty->fetch('008_oldFormat_1.tpl'));
        $this->assertEquals("a9b8c7d6", $this->smarty->fetch('009_oldFormat_1.tpl'));
        $this->assertEquals("1", $this->smarty->fetch('010_oldFormat_1.tpl'));
        $this->assertEquals("1", $this->smarty->fetch('011_oldFormat_1.tpl'));
    }

    /**
     * test new style of assign tag
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignNew_001()
    {
        $this->assertEquals("1", $this->smarty->fetch('001_newFormat_1.tpl'));
        $this->assertEquals("2", $this->smarty->fetch('001_newFormat_2.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('001_newFormat_3.tpl'));
        $this->assertEquals("bar", $this->smarty->fetch('002_newFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('003_newFormat_1.tpl'));
        $this->assertEquals("4", $this->smarty->fetch('003_newFormat_2.tpl'));
        $this->assertEquals("5", $this->smarty->fetch('003_newFormat_3.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('004_newFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('005_newFormat_1.tpl'));
        $this->assertEquals("9876", $this->smarty->fetch('006_newFormat_1.tpl'));
        $this->assertEquals("a9b8c7d6", $this->smarty->fetch('007_newFormat_1.tpl'));
    }

    /**
     * test array append
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignArrayAppend_001()
    {
        $this->assertEquals("0112", $this->smarty->fetch('001_newAppend_1.tpl'));
    }

    /**
     * test array append 2
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignArrayAppend_002()
    {
        $this->smarty->assign('foo', 1);
        $tpl = $this->smarty->createTemplate('002_newAppend_1.tpl', null, null, $this->smarty, false);
        $this->assertEquals("0112", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate('002_newAppend_2.tpl', null, null, $this->smarty, false);
        $this->assertEquals("1", $this->smarty->fetch($tpl2));
    }

    /**
     * test array append 3
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignArrayAppend_003()
    {
        $this->smarty->assign('foo', 1);
        $tpl = $this->smarty->createTemplate('003_newAppend_1.tpl', null, null, $this->smarty, false);
        $this->assertEquals("0112", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate('003_newAppend_2.tpl', null, null, $this->smarty, false);
        $this->assertEquals("0112", $this->smarty->fetch($tpl2));
    }

    /**
     * test nested array
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignNestedArray_004()
    {
        $this->assertEquals("1", $this->smarty->fetch('004_newNested_1.tpl'));
    }

    /**
     * test no scope
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignNone_001()
    {
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assign('include', '001_scope_none_1.tpl');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($this->smarty);
        $data->assign('foo', 'data');
        $tpl = $this->smarty->createTemplate('001_scope_root.tpl', $data);
        $this->assertContains("template 001_scope_none_1.tpl:var =none\ntemplate 001_scope_include.tpl:var =data\ntemplate 001_scope_root.tpl:var =data\ndata:var =data\nSmarty:var =smarty\nglobal:var =global\n",
                              $tpl->fetch());
        $this->smarty->assign('include', '001_scope_none_2.tpl');
        $this->assertContains("template 001_scope_none_2.tpl:var =none\ntemplate 001_scope_include.tpl:var =data\ntemplate 001_scope_root.tpl:var =data\ndata:var =data\nSmarty:var =smarty\nglobal:var =global\n",
                              $tpl->fetch());
    }

    /**
     * test scope parent
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignParent_001()
    {
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assign('include', '001_scope_parent_1.tpl');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($this->smarty);
        $data->assign('foo', 'data');
        $tpl = $this->smarty->createTemplate('001_scope_root.tpl', null, null, $data, false);
        $this->assertContains("template 001_scope_parent_1.tpl:var =parent\ntemplate 001_scope_include.tpl:var =parent\ntemplate 001_scope_root.tpl:var =data\ndata:var =data\nSmarty:var =smarty\nglobal:var =global\n",
                              $tpl->fetch());
        $this->smarty->assign('include', '001_scope_parent_2.tpl');
        $this->assertContains("template 001_scope_parent_2.tpl:var =parent\ntemplate 001_scope_include.tpl:var =parent\ntemplate 001_scope_root.tpl:var =data\ndata:var =data\nSmarty:var =smarty\nglobal:var =global\n",
                              $tpl->fetch());
    }

    /**
     * test scope tpl_root
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignTplRoot_001()
    {
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assign('include', '001_scope_tpl_root_1.tpl');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($this->smarty);
        $data->assign('foo', 'data');
        $tpl = $this->smarty->createTemplate('001_scope_root.tpl', null, null, $data, false);
        $this->assertContains("template 001_scope_tpl_root_1.tpl:var =tpl_root\ntemplate 001_scope_include.tpl:var =tpl_root\ntemplate 001_scope_root.tpl:var =tpl_root\ndata:var =data\nSmarty:var =smarty\nglobal:var =global\n",
                              $tpl->fetch());
        $this->smarty->assign('include', '001_scope_tpl_root_2.tpl');
        $this->assertContains("template 001_scope_tpl_root_2.tpl:var =tpl_root\ntemplate 001_scope_include.tpl:var =tpl_root\ntemplate 001_scope_root.tpl:var =tpl_root\ndata:var =data\nSmarty:var =smarty\nglobal:var =global\n",
                              $tpl->fetch());
    }

    /**
     * test scope root
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignRoot_001()
    {
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assign('include', '001_scope_root_1.tpl');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($this->smarty);
        $data->assign('foo', 'data');
        $tpl = $this->smarty->createTemplate('001_scope_root.tpl', null, null, $data, false);
        $this->assertContains("template 001_scope_root_1.tpl:var =root\ntemplate 001_scope_include.tpl:var =root\ntemplate 001_scope_root.tpl:var =root\ndata:var =root\nSmarty:var =root\nglobal:var =global\n",
                              $tpl->fetch());
        $this->smarty->assign('include', '001_scope_root_2.tpl');
        $this->smarty->assign('foo', 'smarty');
        $data->assign('foo', 'data');
        $this->assertContains("template 001_scope_root_2.tpl:var =root\ntemplate 001_scope_include.tpl:var =root\ntemplate 001_scope_root.tpl:var =root\ndata:var =root\nSmarty:var =root\nglobal:var =global\n",
                              $tpl->fetch());
    }

    /**
     * test scope root data object
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignRoot_002()
    {
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData();
        $data->assign('foo', 'data');
        $data->assign('include', '001_scope_root_1.tpl');
        $tpl = $this->smarty->createTemplate('001_scope_root.tpl', null, null, $data, false);
        $this->assertContains("template 001_scope_root_1.tpl:var =root\ntemplate 001_scope_include.tpl:var =root\ntemplate 001_scope_root.tpl:var =root\ndata:var =root\nSmarty:var =smarty\nglobal:var =global\n",
                              $tpl->fetch());
        $this->smarty->assign('foo', 'smarty');
        $data->assign('foo', 'data');
        $data->assign('include', '001_scope_root_2.tpl');
        $this->assertContains("template 001_scope_root_2.tpl:var =root\ntemplate 001_scope_include.tpl:var =root\ntemplate 001_scope_root.tpl:var =root\ndata:var =root\nSmarty:var =smarty\nglobal:var =global\n",
                              $tpl->fetch());
    }

    /**
     * test scope global
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignGlobal_001()
    {
        $this->smarty->assign('include', '001_scope_global_1.tpl');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($this->smarty);
        $tpl = $this->smarty->createTemplate('001_scope_root.tpl', null, null, $data, false);
        $this->assertContains("template 001_scope_global_1.tpl:var =new global\ntemplate 001_scope_include.tpl:var =new global\ntemplate 001_scope_root.tpl:var =new global\ndata:var =null\nSmarty:var =null\nglobal:var =new global\n",
                              $tpl->fetch());
        $this->smarty->assign('include', '001_scope_global_2.tpl');
        $this->assertContains("template 001_scope_global_2.tpl:var =new global\ntemplate 001_scope_include.tpl:var =new global\ntemplate 001_scope_root.tpl:var =new global\ndata:var =null\nSmarty:var =null\nglobal:var =new global\n",
                              $tpl->fetch());
    }

    /**
     * test scope global
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignGlobal_002()
    {
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assign('include', '001_scope_global_1.tpl');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($this->smarty);
        $tpl = $this->smarty->createTemplate('001_scope_root.tpl', null, null, $data, false);
        $this->assertContains("template 001_scope_global_1.tpl:var =new global\ntemplate 001_scope_include.tpl:var =smarty\ntemplate 001_scope_root.tpl:var =smarty\ndata:var =null\nSmarty:var =smarty\nglobal:var =new global\n",
                              $tpl->fetch());
        $this->smarty->assign('include', '001_scope_global_2.tpl');
        $this->smarty->assign('foo', 'smarty');
        $this->assertContains("template 001_scope_global_2.tpl:var =new global\ntemplate 001_scope_include.tpl:var =smarty\ntemplate 001_scope_root.tpl:var =smarty\ndata:var =null\nSmarty:var =smarty\nglobal:var =new global\n",
                              $tpl->fetch());
    }
}
