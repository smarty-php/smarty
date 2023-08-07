<?php
/**
 * Smarty PHPunit tests compilation of {while} tag
 *

 * @author  Uwe Tews
 */

/**
 * class for {while} tag tests
 *
 * @not runTestsInSeparateProcess
 * 
 * 
 */
class CompileWhileTest extends PHPUnit_Smarty
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
     * test {while 'condition'} tag
     */
    public function testWhileCondition()
    {
        $tpl = $this->smarty->createTemplate('string:{$x=0}{while $x<10}{$x}{$x=$x+1}{/while}');
        $this->assertEquals("0123456789", $this->smarty->fetch($tpl));
    }

    /**
     * test {while 'statement'} tag
     */
    public function testWhileStatement()
    {
        $tpl = $this->smarty->createTemplate('string:{$x=5}{while $x=$x-1}{$x}{/while}');
        $this->assertEquals("4321", $this->smarty->fetch($tpl));
    }

    /**
     * Test spacings
     *
     * 
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 3);
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
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
        return array(array("A{while \$foo=\$foo-1}{\$foo}{/while}C", "A21C", 'T1', $i++),
                     array("A{while \$foo=\$foo-1}\n{\$foo}{/while}C", "A21C", 'T2', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo}\n{/while}C", "A2\n1\nC", 'T3', $i++),
                     array("A{while \$foo=\$foo-1}\n{\$foo}\n{/while}C", "A2\n1\nC", 'T4', $i++),
                     array("A\n{while \$foo=\$foo-1}{\$foo}{/while}C", "A\n21C", 'T5', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo}{/while}\nC", "A21\nC", 'T6', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo}{break}{/while}C", "A2C", 'T7', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo}\n{break}{/while}C", "A2\nC", 'T8', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo}{break}\n{/while}C", "A2C", 'T9', $i++),
                     array("A{while \$foo=\$foo-1}{if \$foo === 2}{continue}{/if}{\$foo}{/while}C", "A1C", 'T10', $i++),
                     array("A{while \$foo=\$foo-1}{if \$foo === 2}{continue}\n{/if}{\$foo}{/while}C", "A1C", 'T11', $i++),
                     array("A{while \$foo=\$foo-1}{if \$foo === 2}\n{continue}\n{/if}{\$foo}{/while}C", "A1C", 'T12', $i++),
                     array("A{while \$foo=\$foo-1}\n{continue}{\$foo}{/while}C", "AC", 'T13', $i++),
                     array("A{while \$foo=\$foo-1}{continue}\n{\$foo}{/while}C", "AC", 'T14', $i++),
                     array("A{while \$foo=\$foo-1}{break}\n{\$foo}{/while}C", "AC", 'T15', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo}{continue}{/while}C", "A21C", 'T16', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo}{continue}\n{/while}C", "A21C", 'T17', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo}\n{continue}\n{/while}C", "A2\n1\nC", 'T18', $i++),
                     array("A{while \$foo=\$foo-1}{\$foo} {continue} {/while}C", "A2 1 C", 'T19', $i++),
                     array("A{while \$foo=\$foo-1} {continue}{\$foo}{/while}C", "A  C", 'T20', $i++),
                     array("A{while \$foo=\$foo-1} {/while}C", "A  C", 'T21', $i++),
                     array("A{while !isset(\$x)}{\$foo}{\$x=1}{/while}C", "A3C", 'T22', $i++),
        );
    }
    /**
     * Test  nocache
     *
     * 
     * 
     * @dataProvider        dataTestNocache
     */
    public function testNocache($value, $nocache, $code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Nocache_{$name}.tpl";
        if ($code) {
            $this->makeTemplateFile($file, $code);
        }
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', $value, $nocache);
        $this->assertEquals($result, $this->smarty->fetch($file),
                            "{$file} - {$testNumber}");
    }

    /*
      * Data provider for testNocache
      */
    public function dataTestNocache()
    {
        $i = 1;
        /*
         * var
         * value
                    * Code
                    * result
                    * test name
                    */
        return array(array(3,false,  "{while \$foo=\$foo-1}\n{\$foo}{/while}", '21', 'T1', $i ++),
                     array(0,false,  false, '21', 'T1', $i ++),
                     array(3,false,  "{while \$foo=\$foo-1 nocache}\n{\$foo}{/while}", '21', 'T2', $i ++),
                     array(4,false,  false, '321', 'T2', $i ++),
                     array(4,true,  "{while \$foo=\$foo-1}\n{\$foo}{/while}", '321', 'T3', $i ++),
                     array(5,true,  false, '4321', 'T3', $i ++),
                     array(array(1,2,3),false,  "{\$x=1}{while isset(\$foo[\$x])}\n{\$foo[\$x]}{\$x++}{/while}", '2132', 'T4', $i ++),
                     array(array(3,4,5),false,  false, '2132', 'T4', $i ++),
                     array(array(1,2,3),false,  "{\$x=1 nocache}{while isset(\$foo[\$x])}\n{\$foo[\$x]}{\$x++}{/while}", '2132', 'T5', $i ++),
                     array(array(3,4,5),false,  false, '4152', 'T5', $i ++),
                     array(array(1,2,3),true,  "{\$x=1 nocache}{while isset(\$foo[\$x])}\n{\$foo[\$x]}{\$x++}{/while}", '2132', 'T6', $i ++),
                     array(array(3,4,5),true,  false, '4152', 'T6', $i ++),
                     array(array(1,2,3,4,5),true,  "{\$x=0 nocache}{while \$foo[\$x] <= 3}\n{\$foo[\$x]}{\$x++}{/while}", '102132', 'T7', $i ++),
                     array(array(2,3,4,5),true,  false, '2031', 'T7', $i ++),
                     array(array(1,2,false,4,5),true,  "{\$x=0 nocache}{while \$foo[\$x]}\n{\$foo[\$x]}{\$x++}{/while}", '1021', 'T8', $i ++),
                     array(array(2,false,4,5),true,  false, '20', 'T8', $i ++),
        );
     }

}
