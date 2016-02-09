<?php
/**
 * Smarty PHPunit tests compilation of append tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for append tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileAppendTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("../../../__shared/templates/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test {append} tags
     *
     * @not runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestAppend
     */
    public function testAppend($code, $result, $testName, $testNumber)
    {
        $file = "testAppend_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->assertEquals($this->strip($result), $this->strip($this->smarty->fetch($file)), "testAppend - {$code} - {$testName}");
    }

    /*
      * Data provider für testAppend
      */
    public function dataTestAppend()
    {
        $i = 0;
        /*
                    * Code
                    * result
                    * test name
                    */
        return array(// old format
            array('{$foo=1}{append var=foo value=2}{$foo|var_export:true}', 'array(0=>1,1=>2,)', '', $i ++),
            array('{append var=foo value=2}{$foo|var_export:true}', 'array(0=>2,)', '', $i ++),
            array('{append foo value=3}{$foo|var_export:true}', 'array(0=>3,)', '', $i ++),
            array('{append foo 5}{$foo|var_export:true}', 'array(0=>5,)', '', $i ++), // new format
            array('{$foo[]=2}{$foo|var_export:true}', 'array(0=>2,)', '', $i ++),
        );
    }

    /**
     * Test scope
     *
     * @run                 InSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestScope
     */
    public function testScope($code, $useSmarty, $result, $testName, $testNumber)
    {
        $file = "testScope_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code . '{checkvar var=foo}');
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('foo', 'smarty');
        $this->smarty->assignGlobal('foo', 'global');
        $data1 = $this->smarty->createData($useSmarty ? $this->smarty : null);
        $data = $this->smarty->createData($data1);
        $data1->assign('foo', 'data1');
        $data->assign('foo', 'data');
        $this->assertEquals($this->strip($result), $this->strip($this->smarty->fetch('scope_tag.tpl', $data)), "test - {$code} - {$testName}");
    }

    /*
     * Data provider für testscope
     */
    public function dataTestScope()
    {
        $i = 0;
        /*
         * Code
         * use Smarty object
         * result
         * test name
         */
        return array(
            array(
                '{$foo[] = \'newvar\' scope=tpl_root}', true,
                '#testScope_0.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#data:$foo=\'data\'#data:$foo=\'data1\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo[] = \'newvar\' scope=tpl_root bubble_up}', true,
                '#testScope_1.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#scope_include.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#scope_tag.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#data:$foo=\'data\'#data:$foo=\'data1\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{append var=foo value=\'newvar\' scope=tpl_root}', true,
                '#testScope_2.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#data:$foo=\'data\'#data:$foo=\'data1\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{append var=foo value=\'newvar\' scope=tpl_root bubble_up}', true,
                '#testScope_3.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#scope_include.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#scope_tag.tpl:$foo=array(0=>\'data\',1=>\'newvar\',)#data:$foo=\'data\'#data:$foo=\'data1\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ),
        );
    }

}
