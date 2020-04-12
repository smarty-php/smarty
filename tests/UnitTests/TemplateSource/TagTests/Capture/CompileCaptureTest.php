<?php
/**
 * Smarty PHPunit tests compilation of capture tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for capture tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileCaptureTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addTemplateDir("./templates_tmp");
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test capture tags
     *
     * @not                 runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestCapture
     */
    public function testCapture($code, $result, $testName, $testNumber)
    {
        $file = "testCapture{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->assignGlobal('file', $file);
         $this->assertEquals($result, $this->smarty->fetch($file), "testCapture - {$code} - {$testName}");
    }

    /*
      * Data provider für testCapture
      */
    public function dataTestCapture()
    {
        $i = 1;
        /*
        * Code
        * result
        * test name
        */
        return array(// old format
                     array('{assign var=foo value=bar}{capture assign=foo}hello world{/capture}{$foo}', 'hello world', '', $i ++),
                     array('{capture name=foo}hello world{/capture}{$smarty.capture.foo}', 'hello world', '', $i ++),
                     array('{capture name=foo assign=bar}hello world{/capture}{$smarty.capture.foo} {$bar}', 'hello world hello world', '', $i ++),
                     array('{capture}hello world{/capture}{$smarty.capture.default}', 'hello world', '', $i ++),
                     array('{capture short}hello shorttag{/capture}{$smarty.capture.short}', 'hello shorttag', '', $i ++),
                     array('{capture append=foo}hello{/capture}bar{capture append=foo}world{/capture}{foreach $foo item} {$item@key} {$item}{/foreach}', 'bar 0 hello 1 world', '', $i ++),
                     array('{capture assign=foo}hello {capture assign=bar}this is my {/capture}world{/capture}{$foo} {$bar}', 'hello world this is my ', '', $i ++),
                     array('{capture name=foo}hello world{/capture}{capture name=Foo}Smarty 3{/capture}{$smarty.capture.foo} {$smarty.capture.Foo}', 'hello world Smarty 3', '', $i ++),
                     );
    }
    /*
     *  Test that capture results are global
     */
    public function testCapture9()
    {
        $result = $this->smarty->fetch('009_capture.tpl');
        $this->assertContains('-->hello world<--', $result);
        $this->assertContains('-->hello world2<--', $result);
    }

    public function testCompileCaptureNocache1()
    {
        $this->smarty->assign('foo', 1);
        $this->smarty->caching = 1;
        $this->assertContains('foo 1', $this->smarty->fetch('test_capture_nocache.tpl'));
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileCaptureNocache2()
    {
        $this->smarty->assign('foo', 2);
        $this->smarty->caching = 1;
        $this->assertTrue($this->smarty->isCached('test_capture_nocache.tpl'));
        $this->assertContains('foo 2', $this->smarty->fetch('test_capture_nocache.tpl'));
    }
    /*
     *  Test capture buffer names with uppercase
     */
    public function testCapture10()
    {
        $result = $this->smarty->fetch('010_capture.tpl');
        $this->assertContains('lowercase', $result);
        $this->assertContains('uppercase', $result);
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
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "Spacing - {$file}");
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
        return array(array("A{capture}B{/capture}C{\$smarty.capture.default}", "ACB", 'Newline1', $i++),
                     array("A{capture}\nB{/capture}C{\$smarty.capture.default}", "ACB", 'Newline2', $i++),
                     array("A{capture}B\n{/capture}C{\$smarty.capture.default}", "ACB\n", 'Newline3', $i++),
                     array("A\n{capture}\nB\n{/capture}C{\$smarty.capture.default}", "A\nCB\n", 'Newline4', $i++),
                     array("{capture}B{/capture}A{\$smarty.capture.default}C", "ABC", 'Newline5', $i++),
                     array("{capture}B{/capture}A\n{\$smarty.capture.default}C", "A\nBC", 'Newline6', $i++),
                     array("{capture}B{/capture}A{\$smarty.capture.default}\nC", "AB\nC", 'Newline7', $i++),
                     array("{capture}B{/capture}A\n{\$smarty.capture.default}\nC", "A\nB\nC", 'Newline8', $i++),
        );
   }

}
