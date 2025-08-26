<?php
/**
 * Smarty PHPunit tests compilation of append tags
 *

 * @author  Uwe Tews
 */

/**
 * class for append tags tests
 *
 *
 * @preserveGlobalState    disabled
 * 
 */
class CompileAppendTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("../../../__shared/templates/");
        $this->smarty->addTemplateDir("./templates_tmp");
        $this->smarty->registerPlugin('modifier', 'var_export', 'var_export');
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test {append} tags
     *
     * 
     *
     * @dataProvider        dataTestAppend
     */
    public function testAppend($code = '{$foo=1}{append var=foo value=2}{$foo|var_export:true}', $result = 'array (
  0 => 1,
  1 => 2,
)', $testName = '', $testNumber = 0)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Append_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
        $this->assertEquals($result, $this->smarty->fetch($file), "testAppend - {$code} - {$testName}");
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
                     array('{$foo=1}{append var=foo value=2}{$foo|var_export:true}', var_export(array(0=>1,1=>2,),true), '', $i ++),
                     array('{$foo[\'i\']=1}{append var=foo value=2 index=\'j\'}{$foo|var_export:true}', var_export(array('i'=>1,'j'=>2,),true), '', $i ++),
                     array('{$bar=\'j\'}{$foo[\'i\']=1}{append var=foo value=2 index=$bar}{$foo|var_export:true}', var_export(array('i'=>1,'j'=>2,),true), '', $i ++),
                     array('{append var=foo value=2}{$foo|var_export:true}', var_export(array(0=>2,),true), '', $i ++),
                     array('{append foo value=3}{$foo|var_export:true}', var_export(array(0=>3,),true), '', $i ++),
                     array('{append foo 5}{$foo|var_export:true}', var_export(array(0=>5,),true), '', $i ++), // new format
                     array('{$foo[]=2}{$foo|var_export:true}', var_export(array(0=>2,),true), '', $i ++),
        );
    }

    /**
     * Test Assign spacings
     *
     *
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testAppendSpacing($code = "A{append var=buh value=\$foo}B{\$buh[0]}", $result = "ABbar", $testName = 'Text', $testNumber = 1)
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
     * Test Append nocache spacings
     *
     *
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testAppendSpacingNocache($code = "A{append var=buh value=\$foo}B{\$buh[0]}", $result = "ABbar", $testName = 'Text', $testNumber = 1)
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
     * Test Append nocache spacings
     *
     *
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testAppendSpacingNocache2($code = "A{append var=buh value=\$foo}B{\$buh[0]}", $result = "ABbar", $testName = 'Text', $testNumber = 1)
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
            array("A{append var=buh value=\$foo}B{\$buh[0]}", "ABbar", 'Text', $i++),
            array("A\n{append var=buh value=\$foo}\nB{\$buh[0]}", "A\nBbar", 'Newline1', $i++),
            array("E{append var=buh value=\$foo}\nF{\$buh[0]}", "EFbar", 'Newline2', $i++),
            array("G\n{append var=buh value=\$foo}H{\$buh[0]}", "G\nHbar", 'Newline3', $i++),
            array("A{\$buh[]=\$foo}B{\$buh[0]}", "ABbar", '2_Text', $i++),
            array("A\n{\$buh[]=\$foo}\nB{\$buh[0]}", "A\nBbar", '2_Newline1', $i++),
            array("E{\$buh[]=\$foo}\nF{\$buh[0]}", "EFbar", '2_Newline2', $i++),
            array("G\n{\$buh[]=\$foo}H{\$buh[0]}", "G\nHbar", '2_Newline3', $i++),
        );
    }
}
