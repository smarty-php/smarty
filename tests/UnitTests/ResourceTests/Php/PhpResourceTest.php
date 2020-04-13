<?php
/**
 * Smarty PHPunit tests for PHP resources
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for PHP resource tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class PhpResourceTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    protected function relative($path)
    {
        $path = str_replace(str_replace("\\", "/", dirname(__FILE__)), '.', str_replace("\\", "/", $path));

        return $path;
    }

    /**
     * test getTemplateFilepath
     */
    public function testGetTemplateFilepath()
    {
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertEquals($this->normalizePath("./templates/phphelloworld.php"), $tpl->source->filepath);
    }

    /**
     * test getTemplateTimestamp
     */
    public function testGetTemplateTimestamp()
    {
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertTrue(is_integer($tpl->source->getTimeStamp()));
        $this->assertEquals(10, strlen($tpl->source->getTimeStamp()));
    }

    /**
     * test getTemplateSource
     *-/
     * public function testGetTemplateSource()
     * {
     * $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
     * $this->assertContains('php hello world', $tpl->source->getContent());
     * }
     * /**
     * test usesCompiler
     */
    public function testUsesCompiler()
    {
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertTrue($tpl->source->handler->uncompiled);
    }

    /**
     * test isEvaluated
     */
    public function testIsEvaluated()
    {
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertFalse($tpl->source->handler->recompiled);
    }

    /**
     * test mustCompile
     */
    public function testMustCompile()
    {
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertFalse($tpl->mustCompile());
    }

    /**
     * test getCachedFilepath
     */
    public function testGetCachedFilepath()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $expected = $this->buildCachedPath($tpl, false, null, null, 'phphelloworld.php', 'php',
                                           $this->smarty->getTemplateDir(0), 'file');
        $this->assertEquals($expected, $tpl->cached->filepath);
    }

    /**
     * test create cache file used by the following tests
     */
    public function testCreateCacheFile()
    {
        // create dummy cache file
        $this->smarty->setAllowPhpTemplates(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertContains('php hello world', $this->smarty->fetch($tpl));
    }

    /**
     * test getCachedTimestamp caching enabled
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testGetCachedTimestamp()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertTrue(is_integer($tpl->cached->timestamp));
        $this->assertEquals(10, strlen($tpl->cached->timestamp));
    }

    /**
     * test isCached
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCached()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 10000;
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertTrue($tpl->isCached());
    }

    /**
     * test isCached caching disabled
     */
    public function testIsCachedCachingDisabled()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertFalse($tpl->isCached());
    }

    /**
     * test isCached on touched source
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedTouchedSourcePrepare()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        sleep(2);
        touch($tpl->source->filepath);
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedTouchedSource()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertFalse($tpl->isCached());
    }

    /**
     * test is cache file is written
     */
    public function testWriteCachedContent()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->smarty->fetch($tpl);
        $this->assertTrue(file_exists($tpl->cached->filepath));
    }

    /**
     * test getRenderedTemplate
     */
    public function testGetRenderedTemplate()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertContains('php hello world', $tpl->fetch());
    }

    /**
     * test $smarty->is_cached
     */
    public function testSmartyIsCachedPrepare()
    {
        // clean up for next tests
        $this->cleanCacheDir();
        $this->smarty->setAllowPhpTemplates(true);
        // prepare files for next test
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->smarty->fetch($tpl);
    }

    /**
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testSmartyIsCached()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertTrue($this->smarty->isCached($tpl));
    }

    /**
     * test $smarty->is_cached  caching disabled
     */
    public function testSmartyIsCachedCachingDisabled()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $tpl = $this->smarty->createTemplate('php:phphelloworld.php');
        $this->assertFalse($this->smarty->isCached($tpl));
    }

    public function testGetTemplateFilepathName()
    {
        $this->smarty->addTemplateDir('./templates_2', 'foo');
        $tpl = $this->smarty->createTemplate('php:[foo]helloworld.php');
        $this->assertEquals('./templates_2/helloworld.php', $this->relative($tpl->source->filepath));
    }

    public function testGetCachedFilepathName()
    {
        $this->smarty->addTemplateDir('./templates_2', 'foo');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('php:[foo]helloworld.php');
        $path = $tpl->cached->filepath;
        $expected = $this->buildCachedPath($tpl, false, null, null, 'helloworld.php', 'php',
                                           $this->smarty->getTemplateDir('foo'), 'file');
        $this->assertEquals($expected, $path);
    }

    /**
     * test {include} php resource
     */
    public function testIncludePhpTemplate()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $this->assertContains('php hello world', $this->smarty->fetch('includephp.tpl'));
    }

    /**
     * test {include} php resource caching
     */
    public function testIncludePhpTemplateCaching()
    {
        $this->smarty->caching = true;
        $this->smarty->setAllowPhpTemplates(true);
        $this->assertContains('php hello world', $this->smarty->fetch('includephp.tpl'));
    }

    /**
     * test clearCompiledTemplate()
     */
    public function testClearCompiled()
    {
        $this->smarty->setAllowPhpTemplates(true);
        $this->assertEquals(0, $this->smarty->clearCompiledTemplate('php:phphelloworld.php'));
    }

}
