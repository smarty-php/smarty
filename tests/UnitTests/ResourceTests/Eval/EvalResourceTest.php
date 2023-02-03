<?php
/**
 * Smarty PHPunit tests for eval resources
 *

 * @author  Uwe Tews
 */

/**
 * class for eval resource tests
 *
 *
 * 
 *
 */
class EvalResourceTest extends PHPUnit_Smarty
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
     * test template eval exits
     */
    public function testTemplateEvalExists1()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo}');
        $this->assertTrue($tpl->getSource()->exists);
    }

    public function testTemplateEvalExists2()
    {
        $this->assertTrue($this->smarty->templateExists('eval:{$foo}'));
    }

    public function testGetTemplateResourceName()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertEquals('hello world', $tpl->getSource()->getResourceName());
    }

    public function testGetTemplateTimestamp()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertTrue($tpl->getSource()->getTimeStamp());
    }

    /**
     * test getTemplateSource
     */
    public function testGetTemplateSource()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world{$foo}');
        $this->assertEquals('hello world{$foo}', $tpl->getSource()->getContent());
    }

    /**
     * test empty templates
     */
    public function testEmptyTemplate()
    {
        $tpl = $this->smarty->createTemplate('eval:');
        $this->assertEquals('', $this->smarty->fetch($tpl));
    }

    /**
     * test usesCompiler
     */
    public function testUsesCompiler()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->markTestIncomplete();
    }

    /**
     * test isEvaluated
     */
    public function testIsEvaluated()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertTrue($tpl->getSource()->handler->recompiled);
    }

    /**
     * test mustCompile
     */
    public function testMustCompile()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertTrue($tpl->mustCompile());
    }

    /**
     * test getCompiledFilepath
     */
    public function testGetCompiledFilepath()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertNull($tpl->getCompiled()->filepath);
    }

    /**
     * test getCompiledTimestamp
     */
    public function testGetCompiledTimestamp()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertFalse($tpl->getCompiled()->getTimeStamp());
    }

    /**
     * test isCached
     */
    public function testIsCached()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertFalse($tpl->isCached());
    }

    /**
     * test getRenderedTemplate
     */
    public function testGetRenderedTemplate()
    {
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertEquals('hello world', $tpl->fetch());
    }

    /**
     * test that no complied template and cache file was produced
     */
    public function testNoFiles()
    {
        $this->cleanDir($this->smarty->getCacheDir());
        $this->cleanDir($this->smarty->getCompileDir());
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 20;
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertEquals('hello world', $this->smarty->fetch($tpl));
        $this->assertEquals(0, $this->smarty->clearAllCache());
        $this->assertEquals(0, $this->smarty->clearCompiledTemplate());
    }

    /**
     * test $smarty->is_cached
     */
    public function testSmartyIsCached()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 20;
        $tpl = $this->smarty->createTemplate('eval:hello world');
        $this->assertEquals('hello world', $this->smarty->fetch($tpl));
        $this->assertFalse($this->smarty->isCached($tpl));
    }

    public function testUrlencodeTemplate()
    {
        $tpl = $this->smarty->createTemplate('eval:urlencode:%7B%22foobar%22%7Cescape%7D');
        $this->assertEquals('foobar', $tpl->fetch());
    }

    public function testBase64Template()
    {
        $tpl = $this->smarty->createTemplate('eval:base64:eyJmb29iYXIifGVzY2FwZX0=');
        $this->assertEquals('foobar', $tpl->fetch());
    }

    /**
     * test clearCompiledTemplate()
     */
    public function testClearCompiled()
    {
         $this->assertEquals(0, $this->smarty->clearCompiledTemplate('eval:hello world'));
    }

}
