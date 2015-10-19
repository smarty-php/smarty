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
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test capture tag
     */
    public function testCapture1()
    {
        $tpl = $this->smarty->createTemplate('eval:{capture assign=foo}hello world{/capture}');
        $this->assertEquals("", $this->smarty->fetch($tpl));
    }

    public function testCapture2()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=bar}{capture assign=foo}hello world{/capture}{$foo}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    public function testCapture3()
    {
        $tpl = $this->smarty->createTemplate('eval:{capture name=foo}hello world{/capture}{$smarty.capture.foo}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    public function testCapture4()
    {
        $tpl = $this->smarty->createTemplate('eval:{capture name=foo assign=bar}hello world{/capture}{$smarty.capture.foo} {$bar}');
        $this->assertEquals("hello world hello world", $this->smarty->fetch($tpl));
    }

    public function testCapture5()
    {
        $tpl = $this->smarty->createTemplate('eval:{capture}hello world{/capture}{$smarty.capture.default}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    public function testCapture6()
    {
        $tpl = $this->smarty->createTemplate('eval:{capture short}hello shorttag{/capture}{$smarty.capture.short}');
        $this->assertEquals("hello shorttag", $this->smarty->fetch($tpl));
    }

    public function testCapture7()
    {
        $tpl = $this->smarty->createTemplate('eval:{capture append=foo}hello{/capture}bar{capture append=foo}world{/capture}{foreach $foo item} {$item@key} {$item}{/foreach}');
        $this->assertEquals("bar 0 hello 1 world", $this->smarty->fetch($tpl));
    }

    /*
    *  The following test has been disabled. It fails only in PHPunit
    */
    public function testCapture8()
    {
        $tpl = $this->smarty->createTemplate('eval:{capture assign=foo}hello {capture assign=bar}this is my {/capture}world{/capture}{$foo} {$bar}');
        $this->assertEquals("hello world this is my ", $this->smarty->fetch($tpl), 'This failure pops up only during PHPunit test ?????');
    }

    public function testCompileCaptureNocache1()
    {
        $this->smarty->assign('foo', 1);
        $this->smarty->caching = 1;
        $this->assertContains('foo 1', $this->smarty->fetch('test_capture_nocache.tpl'));
    }

    /**
     *
     * @run InSeparateProcess
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
}
