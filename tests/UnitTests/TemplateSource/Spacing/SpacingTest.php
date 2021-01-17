<?php
/**
 * Smarty PHPunit tests spacing in template output
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for spacing test
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class SpacingTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->template_dir = './templates_tmp';
        $this->smarty->assign('file', $file);
        $this->smarty->assign('foo', 'bar');
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
        return array(array('{$foo}', 'bar', 'T1', $i++),
                     array('{$foo}{$foo}', 'barbar', 'T2', $i++),
                     array('A{$foo}{$foo}B', 'AbarbarB', 'T3', $i++),
                     array('{$foo} {$foo}', 'bar bar', 'T4', $i++),
                     array('A{$foo}B', 'AbarB', 'T5', $i++),
                     array('A{counter}B', 'A1B', 'T6', $i++),
                     array('A {$foo}B', 'A barB', 'T7', $i++),
                     array('A{$foo} B', 'Abar B', 'T8', $i++),
                     array("A{\$foo}\nB", "Abar\nB", 'T9', $i++),
                     array("A{counter start=1}\nB", "A1\nB", 'T10', $i++),
                     array("A{\$foo}B\nC", "AbarB\nC", 'T11', $i++),
                     array("A{assign var=zoo value='blah'}B", "AB", 'T12', $i++),
                     array("A\n{assign var=zoo value='blah'}\nB", "A\nB", 'T13', $i++),
                     array("E{assign var=zoo value='blah'}\nF", "EF", 'T14', $i++),
                     array("G\n{assign var=zoo value='blah'}H", "G\nH", 'T15', $i++),
        );
    }
}
