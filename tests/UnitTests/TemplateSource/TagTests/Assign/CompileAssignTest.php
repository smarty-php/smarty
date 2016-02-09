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
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("../../../__shared/templates/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test assign tags
     *
     * @not                 runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestAssign
     */
    public function testAssign($code, $result, $testName, $testNumber)
    {
        $file = "testAssign_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('bar', 'buh');
        $this->assertEquals($this->strip($result), $this->strip($this->smarty->fetch($file)), "testAssign - {$code} - {$testName}");
    }

    /*
      * Data provider für testAssign
      */
    public function dataTestAssign()
    {
        $i = 1;
        /*
        * Code
        * result
        * test name
        */
        return array(// old format
            array('{assign var=foo value=1}{$foo}', '1', '', $i ++),
            array('{assign var=\'foo\' value=2}{$foo}', '2', '', $i ++),
            array('{assign var="foo" value=3}{$foo}', '3', '', $i ++),
            array('{assign var=foo value=$bar}{$foo}', 'buh', '', $i ++),
            array('{assign var=$bar value=11}{$buh}', '11', '', $i ++),
            array('{assign var=foo value=bar}{$foo}', 'bar', '', $i ++),
            array('{assign var=foo value=1+2}{$foo}', '3', '', $i ++),
            array('{assign var=foo value=strlen(\'barbuh\')}{$foo}', '6', '', $i ++),
            array('{assign var=foo value=\'barr\'|strlen}{$foo}', '4', '', $i ++),
            array('{assign var=foo value=[9,8,7,6]}{$foo|var_export:true}', 'array(0=>9,1=>8,2=>7,3=>6,)', '', $i ++),
            array(
                '{assign var=foo value=[\'a\'=>9,\'b\'=>8,\'c\'=>7,\'d\'=>6]}{$foo|var_export:true}',
                'array(\'a\'=>9,\'b\'=>8,\'c\'=>7,\'d\'=>6,)', '', $i ++,
            ), array('{assign foo  value=1}{$foo}', '1', '', $i ++), array('{assign foo 1}{$foo}', '1', '', $i ++),
            // new format
            array('{$foo=1}{$foo}', '1', '', $i ++), array('{$foo =2}{$foo}', '2', '', $i ++),
            array('{$foo=bar}{$foo}', 'bar', '', $i ++), array('{$foo=1+2}{$foo}', '3', '', $i ++),
            array('{$foo = 1+3}{$foo}', '4', '', $i ++), array('{$foo = 1 + 4}{$foo}', '5', '', $i ++),
            array('{$foo=strlen(\'bar\')}{$foo}', '3', '', $i ++), array('{$foo=\'bar\'|strlen}{$foo}', '3', '', $i ++),
            array('{$foo[\'a\'][4]=1}{$foo[\'a\'][4]}', '1', '', $i ++),
            array('{$foo=[9,8,7,6]}{$foo|var_export:true}', 'array(0=>9,1=>8,2=>7,3=>6,)', '', $i ++), array(
                '{$foo=[\'a\'=>9,\'b\'=>8,\'c\'=>7,\'d\'=>6]}{$foo|var_export:true}',
                'array(\'a\'=>9,\'b\'=>8,\'c\'=>7,\'d\'=>6,)', '', $i ++,
            ),
        );
    }

    /**
     * Test scope
     *
     * @not                 runInSeparateProcess
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
        $data = $this->smarty->createData($useSmarty ? $this->smarty : null);
        $data->assign('foo', 'data');
        $this->assertEquals($this->strip('#' . $file . $result), $this->strip($this->smarty->fetch('scope_tag.tpl', $data)), "test - {$code} - {$testName}");
    }

    /*
     * Data provider für testscope
     */
    public function dataTestScope()
    {
        $i = 1;
        /*
         * Code
         * use Smarty object
         * result
         * test name
         */
        return array(
            array(
                '{$foo = \'newvar\'}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{assign var=foo value=\'newvar\'}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=local}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=local bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{assign var=foo value=\'newvar\' bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{assign var=foo value=\'newvar\' scope=local}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{assign var=foo value=\'newvar\' scope=local bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=parent}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=parent bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{assign var=foo value=\'newvar\' scope=parent}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{assign var=foo value=\'newvar\'  scope=parent bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=tpl_root}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=tpl_root bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=global}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'newvar\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=global bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'#global:$foo=\'newvar\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=root}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=root bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=root}', false,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                'no smarty', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=root bubble_up}', false,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'#global:$foo=\'global\'',
                'no  smarty', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=smarty}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'newvar\'#global:$foo=\'global\'',
                '', $i ++,
            ), array(
                '{$foo = \'newvar\' scope=smarty bubble_up}', false,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'newvar\'#global:$foo=\'global\'',
                'no  smarty', $i ++,
            ),
        );
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
        $this->assertEquals($this->strip($result), $this->strip($this->smarty->fetch($file)), "test - {$file} {$var}");
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
        return array(
            array(
                'b1', 'test_scope_assignbar.tpl',
                '#test_scope_assignbar.tpl:$foo=\'b1\'#Smarty:$foo=\'smarty\'#global:$foo=\'b1\'',
            ), array(
                'b2', 'test_scope_assignbar.tpl',
                '#test_scope_assignbar.tpl:$foo=\'b2\'#Smarty:$foo=\'smarty\'#global:$foo=\'b2\'',
            ), array(
                'b1', 'test_scope_assignnocache.tpl',
                '#test_scope_assignnocache.tpl:$foo=\'b1\'#Smarty:$foo=\'smarty\'#global:$foo=\'b1\'',
            ), array(
                'b2', 'test_scope_assignnocache.tpl',
                '#test_scope_assignnocache.tpl:$foo=\'b2\'#Smarty:$foo=\'smarty\'#global:$foo=\'b2\'',
            ),
        );
    }

}
