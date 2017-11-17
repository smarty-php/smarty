<?php
/**
 * Smarty PHPunit tests array definitions and access
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for array tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class ArrayTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * Test array access
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestArray
     * @runInSeparateProcess
     */
    public function testArray($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Array_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 3);
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
    }

    /*
      * Data provider für testArray
      */
    public function dataTestArray()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array('{$foo=[1,2,3,4,5]}{foreach $foo as $bar}{$bar}{/foreach}', '12345', 'T1', $i++),
                     array('{$foo=[1,2,3,4,5]}{$foo.0}{$foo.1}{$foo.2}', '123', 'T2', $i++),
                     array('{$foo=[1,2,3,4,5]}{$foo[0]}{$foo[1]}{$foo[2]}', '123', 'T3', $i++),
                     array('{$x=\'d\'}{$foo=[a=>1,\'b\'=>2,"c"=>3,$x=>4]}{$foo[\'a\']}{$foo[\'b\']}{$foo[\'c\']}{$foo[\'d\']}', '1234', 'T4', $i++),
                     array('{$foo=[1,2,[a,b,c],4,5]}{$foo[2][1]}', 'b', 'T5', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{$foo[2][1]+1}', '9', 'T6', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{$foo.2.1+1}', '9', 'T7', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{2+$foo[2][1]}', '10', 'T8', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{2+$foo.2.1}', '10', 'T9', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{$foo[2][0]+$foo[2][1]}', '15', 'T10', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{$foo.2.0+$foo.2.1}', '15', 'T11', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{$x=2}{$y=0}{$foo.$x.$y}', '7', 'T12', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{$x=2}{$foo.$x.0}', '7', 'T13', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{$x=0}{$foo.2.$x}', '7', 'T14', $i++),
                     array('{$foo=[1,2,[7,8,9],4,5]}{$x=[1,0]}{$foo.2.{$x.1}}', '7', 'T15', $i++),
        );
    }
}
