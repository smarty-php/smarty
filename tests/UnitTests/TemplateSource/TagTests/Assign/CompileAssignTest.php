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
}
