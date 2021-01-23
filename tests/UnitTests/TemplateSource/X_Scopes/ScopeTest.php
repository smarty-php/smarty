<?php
/**
 * Smarty PHPunit tests for scopes
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class scope tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class ScopeTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("../../__shared/templates/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test scope
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestAppendScope
     */
    public function testAppendScope($code, $useSmarty, $result, $testName, $testNumber)
    {
        $file = "testAppendScope_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code . '{checkvar var=foo}');
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assignGlobal('foo', 'global');
        $data1 = $this->smarty->createData($useSmarty ? $this->smarty : null);
        $data = $this->smarty->createData($data1);
        $data1->assign('foo', 'data1');
        $data->assign('foo', 'data');
        $this->assertEquals($result, $this->smarty->fetch('scope_tag.tpl', $data),
                            "test - {$code} - {$testName}");
    }

    /*
     * Data provider für testAppendScope
     */
    public function dataTestAppendScope()
    {
        $i = 0;
        /*
         * Code
         * use Smarty object
         * result
         * test name
         */
        return array(array('{$foo[] = \'newvar\' scope=tpl_root}', true,
                           '#testAppendScope_0.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#scope_include.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#scope_tag.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#data:$foo =\'data\'#data:$foo =\'data1\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                           '', $i ++,),
                     array('{append var=foo value=\'newvar\' scope=tpl_root}', true,
                                              '#testAppendScope_1.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#scope_include.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#scope_tag.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#data:$foo =\'data\'#data:$foo =\'data1\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                              '', $i ++,),
                     array('{append var=foo value=\'newvar\' scope=global}', true,
                                                                 '#testAppendScope_2.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#scope_include.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#scope_tag.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#data:$foo =\'data\'#data:$foo =\'data1\'#Smarty:$foo =\'smarty\'#global:$foo =array(0=>\'data\',1=>\'newvar\',)',
                                                                 '', $i ++,),
                     array('{append var=foo value=\'newvar\' scope=smarty}', true,
                           '#testAppendScope_3.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#scope_include.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#scope_tag.tpl:$foo =array(0=>\'data\',1=>\'newvar\',)#data:$foo =\'data\'#data:$foo =\'data1\'#Smarty:$foo =array(0=>\'data\',1=>\'newvar\',)#global:$foo =\'global\'',
                           '', $i ++,),);
    }

    /**
     * Test scope
     *
     * @not                 runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestAssignScope
     */
    public function testAssignScope($code, $useSmarty, $result, $testName, $testNumber)
    {
        $file = "testAssignScope_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code . '{checkvar var=foo}');
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($useSmarty ? $this->smarty : null);
        $data->assign('foo', 'data');
        $this->assertEquals('#' . $file . $result,
                            $this->smarty->fetch('scope_tag.tpl', $data), "test - {$code} - {$testName}");
    }

    /*
     * Data provider für testAssignScope
     */
    public function dataTestAssignScope()
    {
        $i = 0;
        /*
         * Code
         * use Smarty object
         * result
         * test name
         */
        return array(array('{$foo = \'newvar\'}', true,
                           ':$foo =\'newvar\'#scope_include.tpl:$foo =\'data\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                           '', $i ++,), array('{assign var=foo value=\'newvar\'}', true,
                                              ':$foo =\'newvar\'#scope_include.tpl:$foo =\'data\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                              '', $i ++,), array('{$foo = \'newvar\' scope=local}', true,
                                                                 ':$foo =\'newvar\'#scope_include.tpl:$foo =\'data\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                                                 '', $i ++,),
                     array('{assign var=foo value=\'newvar\' scope=local}', true,
                           ':$foo =\'newvar\'#scope_include.tpl:$foo =\'data\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                           '', $i ++,), array('{$foo = \'newvar\' scope=parent}', true,
                                              ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                              '', $i ++,), array('{assign var=foo value=\'newvar\' scope=parent}', true,
                                                                 ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                                                 '', $i ++,),
                     array('{$foo = \'newvar\' scope=tpl_root}', true,
                           ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                           '', $i ++,), array('{$foo = \'newvar\' scope=global}', true,
                                              ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'newvar\'',
                                              '', $i ++,), array('{$foo = \'newvar\' scope=root}', true,
                                                                 ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'newvar\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                                                 '', $i ++,),
                     array('{$foo = \'newvar\' scope=root}', false,
                           ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'newvar\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                           'no  smarty', $i ++,), array('{$foo = \'newvar\' scope=smarty}', false,
                                                        ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'newvar\'#global:$foo =\'global\'',
                                                        'no  smarty', $i ++,),);
    }

    /**
     * Test scope nocache
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestScopeNocache
     */
    public function testScopeNocache($var, $file, $result)
    {
        $this->smarty->setCaching(true);
        $this->smarty->assign('bar', $var, true);
        $this->smarty->assign('buh', $var);
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assignGlobal('foo', 'global');
        $this->assertEquals($result, $this->smarty->fetch($file), "test - {$file} {$var}");
    }

    /*
  * Data provider für testscopenocache
  */
    public function dataTestScopeNocache()
    {
        /*
         * variable value
         * result
         */
        return array(array('b1', 'test_scope_assignbar.tpl',
                           '#test_scope_assignbar.tpl:$foo =\'b1\'#Smarty:$foo =\'smarty\'#global:$foo =\'b1\'',),
                     array('b2', 'test_scope_assignbar.tpl',
                           '#test_scope_assignbar.tpl:$foo =\'b2\'#Smarty:$foo =\'smarty\'#global:$foo =\'b2\'',),
                     array('b1', 'test_scope_assignnocache.tpl',
                           '#test_scope_assignnocache.tpl:$foo =\'b1\'#Smarty:$foo =\'smarty\'#global:$foo =\'b1\'',),
                     array('b2', 'test_scope_assignnocache.tpl',
                           '#test_scope_assignnocache.tpl:$foo =\'b2\'#Smarty:$foo =\'smarty\'#global:$foo =\'b2\'',),);
    }

    /**
     * Test scope
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestIncludeScope
     */
    public function testIncludeScope($code, $useSmarty, $result, $testName, $testNumber = null)
    {
        $file = "testIncludeScope_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assignGlobal('foo', 'global');
        $data = $this->smarty->createData($useSmarty ? $this->smarty : null);
        $data->assign('foo', 'data');
        if (!$useSmarty) {
            $testName .= 'no smarty';
        }
        $this->assertEquals($result, $this->smarty->fetch('test_scope.tpl', $data),
                            "test - {$code} - {$testName}");
    }

    /*
     * Data provider for testIncludeScope
     */
    public function dataTestIncludeScope()
    {
        $i = 0;
        return array(/*
             * Code
             * use Smarty object
             * result
             * test name
             */
                     array('{include \'test_scope_assign.tpl\'}', true,
                           '#test_scope_assign.tpl:$foo =\'newvar\'#testIncludeScope_' . $i .
                           '.tpl:$foo =\'data\'#test_scope.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                           '', $i ++), array('{include \'test_scope_assign.tpl\' scope=parent}', true,
                                             '#test_scope_assign.tpl:$foo =\'newvar\'#testIncludeScope_' . $i .
                                             '.tpl:$foo =\'newvar\'#test_scope.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                             '', $i ++),
                     array('{include \'test_scope_assign.tpl\' scope=tpl_root}', true,
                           '#test_scope_assign.tpl:$foo =\'newvar\'#testIncludeScope_' . $i .
                           '.tpl:$foo =\'newvar\'#test_scope.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                           '', $i ++), array('{include \'test_scope_assign.tpl\' scope=root}', true,
                                             '#test_scope_assign.tpl:$foo =\'newvar\'#testIncludeScope_' . $i .
                                             '.tpl:$foo =\'newvar\'#test_scope.tpl:$foo =\'newvar\'#data:$foo =\'newvar\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                             '', $i ++), array('{include \'test_scope_assign.tpl\' scope=root}', false,
                                                               '#test_scope_assign.tpl:$foo =\'newvar\'#testIncludeScope_' .
                                                               $i .
                                                               '.tpl:$foo =\'newvar\'#test_scope.tpl:$foo =\'newvar\'#data:$foo =\'newvar\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                                               '', $i ++),
                     array('{include \'test_scope_assign.tpl\' scope=smarty}', true,
                           '#test_scope_assign.tpl:$foo =\'newvar\'#testIncludeScope_' . $i .
                           '.tpl:$foo =\'newvar\'#test_scope.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'newvar\'#global:$foo =\'global\'',
                           '', $i ++), array('{include \'test_scope_assign.tpl\' scope=global}', true,
                                             '#test_scope_assign.tpl:$foo =\'newvar\'#testIncludeScope_' . $i .
                                             '.tpl:$foo =\'newvar\'#test_scope.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'newvar\'',
                                             '', $i ++),
                     array('{include \'test_scope_pluginassign.tpl\' scope=global}', true,
                           '#test_scope_pluginassign.tpl:$foo =\'newvar\'#testIncludeScope_' . $i .
                           '.tpl:$foo =\'newvar\'#test_scope.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'newvar\'',
                           '', $i ++), array('{include \'test_scope_assign_noscope.tpl\' scope=root}', true,
                                             '#test_scope_assign_noscope.tpl:$foo =\'newvar\'#testIncludeScope_' . $i .
                                             '.tpl:$foo =\'data\'#test_scope.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'#global:$foo =\'global\'',
                                             '', $i ++),);
    }

    /**
     * Test scope
     *
     * @not                 runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestConfigScope
     */
    public function testConfigScope($code, $useSmarty, $result, $testName, $testNumber)
    {
        $file = "testConfigScope_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code . '{checkconfigvar var=foo}');
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->configLoad('smarty.conf');
        $data = $this->smarty->createData($useSmarty ? $this->smarty : null);
        $data->configLoad('data.conf');
        $this->assertEquals('#' . $file . $result,
                            $this->smarty->fetch('scope_tag.tpl', $data), "test - {$code} - {$testName}");
    }

    /*
     * Data provider für testConfigScope
     */
    public function dataTestConfigScope()
    {
        $i = 0;
        /*
         * Code
         * use Smarty object
         * result
         * test name
         */
        return array(array('{config_load \'template.conf\'}', true,
                           ':$foo =\'newvar\'#scope_include.tpl:$foo =\'data\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'',
                           '', $i ++,), array('{config_load \'template.conf\' scope=local}', true,
                                              ':$foo =\'newvar\'#scope_include.tpl:$foo =\'data\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'',
                                              '', $i ++,), array('{config_load \'template.conf\' scope=parent}', true,
                                                                 ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'data\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'',
                                                                 '', $i ++,),
                     array('{config_load \'template.conf\' scope=tpl_root}', true,
                           ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'smarty\'',
                           '', $i ++,), array('{config_load \'template.conf\' scope=root}', true,
                                              ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'newvar\'#Smarty:$foo =\'smarty\'',
                                              '', $i ++,), array('{config_load \'template.conf\' scope=root}', false,
                                                                 ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'newvar\'#Smarty:$foo =\'smarty\'',
                                                                 'no  smarty', $i ++,),
                     array('{config_load \'template.conf\' scope=smarty}', false,
                           ':$foo =\'newvar\'#scope_include.tpl:$foo =\'newvar\'#scope_tag.tpl:$foo =\'newvar\'#data:$foo =\'data\'#Smarty:$foo =\'newvar\'',
                           'no  smarty', $i ++,),);
    }

    /**
     * @doesNotPerformAssertions
     */
    public function testFunctionScope()
    {
        $this->smarty->assign('scope', 'none');
        $r = $this->smarty->fetch('test_function_scope.tpl');
    }
}
