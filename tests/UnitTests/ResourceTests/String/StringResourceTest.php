<?php
/**
 * Smarty PHPunit tests for string resources
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for string resource tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class StringResourceTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    protected function relative($path)
    {
        $path = str_replace(dirname(__FILE__), '.', $path);
        if (DIRECTORY_SEPARATOR == "\\") {
            $path = str_replace("\\", "/", $path);
        }

        return $path;
    }

    /**
     * test template string exits
     */
    public function testTemplateStringExists1()
    {
        $tpl = $this->smarty->createTemplate('string:{$foo}');
        $this->assertTrue($tpl->source->exists);
    }

    public function testTemplateStringExists2()
    {
        $this->assertTrue($this->smarty->templateExists('string:{$foo}'));
    }

    /**
     * test getTemplateFilepath
     */
    public function testGetTemplateFilepath()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertEquals($this->buildSourcePath($tpl), $tpl->source->filepath);
    }

    /**
     * test getTemplateTimestamp
     */
    public function testGetTemplateTimestamp()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertTrue($tpl->source->getTimeStamp());
    }

    /**
     * test getTemplateSource
     */
    public function testGetTemplateSource()
    {
        $tpl = $this->smarty->createTemplate('string:hello world{$foo}');
        $this->assertEquals('hello world{$foo}', $tpl->source->getContent());
    }

    /**
     * test usesCompiler
     */
    public function testUsesCompiler()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertFalse($tpl->source->handler->uncompiled);
    }

    /**
     * test isEvaluated
     */
    public function testIsEvaluated()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertFalse($tpl->source->handler->recompiled);
    }

    /**
     * test mustCompile
     */
    public function testMustCompile()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertTrue($tpl->mustCompile());
    }

    /**
     * test getCompiledFilepath
     */
    public function testGetCompiledFilepath()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertEquals($this->buildCompiledPath($tpl, false, false, null, 'hello world', 'string', $this->smarty->getTemplateDir(0)), $tpl->compiled->filepath);
    }

    /**
     * test getCompiledTimestamp
     */
    public function testGetCompiledTimestamp()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertFalse($tpl->compiled->getTimeStamp());
    }

    /**
     * test empty templates
     */
    public function testEmptyTemplate()
    {
        $tpl = $this->smarty->createTemplate('string:');
        $this->assertEquals('', $this->smarty->fetch($tpl));
    }

    /**
     * test getCachedTimestamp
     */
    public function testGetCachedTimestamp()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertFalse($tpl->cached->timestamp);
    }

    /**
     * test writeCachedContent
     */
    public function testWriteCachedContent()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertFalse($tpl->writeCachedContent('dummy'));
    }

    /**
     * test isCached
     */
    public function testIsCached()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertFalse($tpl->isCached());
    }

    /**
     * test getRenderedTemplate
     */
    public function testGetRenderedTemplate()
    {
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertEquals('hello world', $tpl->fetch());
    }

    /**
     * test $smarty->is_cached
     */
    public function testSmartyIsCached()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 20;
        $tpl = $this->smarty->createTemplate('string:hello world');
        $this->assertEquals('hello world', $this->smarty->fetch($tpl));
        $this->assertTrue($this->smarty->isCached($tpl));
    }

    public function testUrlencodeTemplate()
    {
        $tpl = $this->smarty->createTemplate('string:urlencode:%7B%22foobar%22%7Cescape%7D');
        $this->assertEquals('foobar', $tpl->fetch());
    }

    public function testBase64Template()
    {
        $tpl = $this->smarty->createTemplate('string:base64:eyJmb29iYXIifGVzY2FwZX0=');
        $this->assertEquals('foobar', $tpl->fetch());
    }
    public function testClearCompiled()
    {
        $this->smarty->fetch('string:string:hello uwe');
        $this->assertEquals(1, $this->smarty->clearCompiledTemplate('string:'));
    }
}
