<?php
/**
 * Smarty PHPunit tests variable output with nocache attribute
 *

 * @author  Uwe Tews
 */

/**
 * class for variable output with nocache attribute tag tests
 *
 *
 *
 * 
 */
class PrintTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * Test Output spacings
     *
     *
     * @dataProvider        dataTestOutputSpacing
     * 
     */
    public function testOutputSpacing($code, $result, $testName, $testNumber)
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
    /**
     * Test Output nocache spacings
     *
     *
     * @dataProvider        dataTestOutputSpacing
     * 
     */
    public function testOutputSpacingNocache($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "testSpacing_{$name}.tpl";
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
     *
     * @dataProvider        dataTestOutputSpacing
     * 
     */
    public function testOutputSpacingNocache2($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "testSpacing_{$name}.tpl";
        $this->smarty->setCompileId('1');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'foo',true);
        $this->assertEquals(str_replace('bar','foo',$result),
                            $this->smarty->fetch($file),
                            "testSpacing - {$file}");
    }

    /*
      * Data provider für testOutputSpacing
      */
    public function dataTestOutputSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array('{$foo}', 'bar', 'Variable', $i++),
                     array('{$foo}{$foo}', 'barbar', 'twoVariables', $i++),
                     array('A{$foo}{$foo}B', 'AbarbarB', 'twoVariablesInText', $i++),
                     array('{$foo} {$foo}', 'bar bar', 'twoVariablesWithSpace', $i++),
                     array('A{$foo}B', 'AbarB', 'VariableInText1', $i++),
                     array('A {$foo}B', 'A barB', 'VariableInText2', $i++),
                     array('A{$foo} B', 'Abar B', 'VariableInText3', $i++),
                     array("A{\$foo}\nB", "Abar\nB", 'VariableInTextNewline1', $i++),
                     array("A{\$foo}B\nC", "AbarB\nC", 'VariableInTextNewline2', $i++),
        );
    }
}
