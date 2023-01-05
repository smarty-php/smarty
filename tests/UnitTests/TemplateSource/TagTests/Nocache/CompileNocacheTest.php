<?php
/**
 * Smarty PHPunit tests compilation of {nocache} tag
 *

 * @author  Uwe Tews
 */

/**
 * class for {nocache} tag tests
 *
 * 
 * 
 *
 */
class CompileNocacheTest extends PHPUnit_Smarty
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
     * test nocache tag caching disabled
     */
    public function testNocacheCachingNo()
    {
        $this->smarty->caching = 0;
        $this->smarty->assign('foo', 0);
        $this->smarty->assign('bar', 'A');
        $content = $this->smarty->fetch('test_nocache_tag.tpl');
        $this->assertStringContainsString("root 2A", $content);
        $this->assertStringContainsString("include 4A", $content);
        $this->smarty->assign('foo', 2);
        $this->smarty->assign('bar', 'B');
        $content = $this->smarty->fetch('test_nocache_tag.tpl');
        $this->assertStringContainsString("root 4B", $content);
        $this->assertStringContainsString("include 6B", $content);
    }

    /**
     * test nocache tag caching enabled
    */
    public function testNocacheCachingYes1()
    {
        $this->smarty->caching = 1;
        $this->smarty->assign('foo', 0);
        $this->smarty->assign('bar', 'A');
        $content = $this->smarty->fetch('test_nocache_tag.tpl');
        $this->assertStringContainsString("root 2A", $content);
        $this->assertStringContainsString("include 4A", $content);

    }

    /**
     *
     * 
     * 
     *
     */
    public function testNocacheCachingYes2()
    {
        $this->smarty->caching = 1;
        $this->smarty->assign('foo', 2);
        $this->smarty->assign('bar', 'B');
        $content = $this->smarty->fetch('test_nocache_tag.tpl');
        $this->assertStringContainsString("root 4A", $content);
        $this->assertStringContainsString("include 6A", $content);
    }
}
