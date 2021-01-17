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
    public function setUp(): void
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
        $file = "Assign_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->assign('bar', 'buh');
        $this->assertEquals($result, $this->smarty->fetch($file), "testAssign - {$code} - {$testName}");
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
                     array('{assign var=foo value=1}{$foo}', '1', '', $i++),
                     array('{assign var=\'foo\' value=2}{$foo}', '2', '', $i++),
                     array('{assign var="foo" value=3}{$foo}', '3', '', $i++),
                     array('{assign var=foo value=$bar}{$foo}', 'buh', '', $i++),
                     array('{assign var=$bar value=11}{$buh}', '11', '', $i++),
                     array('{assign var=foo value=bar}{$foo}', 'bar', '', $i++),
                     array('{assign var=foo value=1+2}{$foo}', '3', '', $i++),
                     array('{assign var=foo value=strlen(\'barbuh\')}{$foo}', '6', '', $i++),
                     array('{assign var=foo value=\'barr\'|strlen}{$foo}', '4', '', $i++),
                     array('{assign var=foo value=[9,8,7,6]}{$foo|var_export:true}',
                           var_export(array(9, 8, 7, 6), true), '', $i++),
                     array(
                         '{assign var=foo value=[\'a\'=>9,\'b\'=>8,\'c\'=>7,\'d\'=>6]}{$foo|var_export:true}',
                         var_export(array('a' => 9, 'b' => 8, 'c' => 7, 'd' => 6,), true), '', $i++,),
                     array('{assign foo  value=1}{$foo}', '1', '', $i++),
                     array('{assign foo 1}{$foo}', '1', '', $i++),
                     // new format
                     array('{$foo=1}{$foo}', '1', '', $i++),
                     array('{$foo =2}{$foo}', '2', '', $i++),
                     array('{$foo=bar}{$foo}', 'bar', '', $i++),
                     array('{$foo=1+2}{$foo}', '3', '', $i++),
                     array('{$foo = 1+3}{$foo}', '4', '', $i++),
                     array('{$foo = 1 + 4}{$foo}', '5', '', $i++),
                     array('{$foo=strlen(\'bar\')}{$foo}', '3', '', $i++),
                     array('{$foo=\'bar\'|strlen}{$foo}', '3', '', $i++),
                     array('{$foo[\'a\'][4]=1}{$foo[\'a\'][4]}', '1', '', $i++),
                     array('{$foo=[9,8,7,6]}{$foo|var_export:true}', var_export(array(9, 8, 7, 6), true), '', $i++),
                     array(
                         '{$foo=[\'a\'=>9,\'b\'=>8,\'c\'=>7,\'d\'=>6]}{$foo|var_export:true}',
                         var_export(array('a' => 9, 'b' => 8, 'c' => 7, 'd' => 6,), true), '', $i++),
        );
    }

    /**
     * Test Assign spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testAssignSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testSpacing - {$file}");
    }
    /**
     * Test Output nocache spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testAssignSpacingNocache($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->smarty->setCompileId('1');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar',true);
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testSpacing - {$file}");
    }
    /**
     * Test Output nocache spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testAssignSpacingNocache2($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->smarty->setCompileId('1');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'foo',true);
        $this->assertEquals(str_replace('bar','foo',$result),
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
        return array(
                     array("A{assign var=buh value=\$foo}B{\$buh}", "ABbar", 'Text1', $i++),
                     array("A\n{assign var=buh value=\$foo}\nB{\$buh}", "A\nBbar", 'Newline1', $i++),
                     array("E{assign var=buh value=\$foo}\nF{\$buh}", "EFbar", 'Newline2', $i++),
                     array("G\n{assign var=buh value=\$foo}H{\$buh}", "G\nHbar", 'Newline3', $i++),
        );
    }
}
