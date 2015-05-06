<?php
/**
 * Smarty PHPunit tests for cache resource file
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for cache resource file tests
 *
 * @backupStaticAttributes enabled
 */
class CacheResourceTestCommon extends PHPUnit_Smarty
{
    public static $touchResource = null;

    public function setUp()
    {
        $this->smarty->setTemplateDir(__DIR__ . '/../_shared/templates');
        $this->smarty->addPluginsDir(__DIR__ . '/../_shared/PHPunitplugins');
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
    }


    protected function doClearCacheAssertion($a, $b)
    {
        $this->assertEquals($a, $b);
    }

    public function compiledPrefilter($text, Smarty_Internal_Template $tpl)
    {
        return str_replace('#', $tpl->getVariable('test'), $text);
    }

    /**
     * test getCachedFilepath with configuration['useSubDirs'] enabled
     */
    public function testGetCachedFilepathSubDirs()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals($this->buildCachedPath($tpl), $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with cache_id
     */
    public function testGetCachedFilepathCacheId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar');
        $this->assertEquals($this->buildCachedPath($tpl, true, 'foo|bar'), $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with compile_id
     */
    public function testGetCachedFilepathCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', null, 'blar');
        $this->assertEquals($this->buildCachedPath($tpl, true, null, 'blar'), $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with cache_id and compile_id
     */
    public function testGetCachedFilepathCacheIdCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->assertEquals($this->buildCachedPath($tpl, true, 'foo|bar', 'blar'), $tpl->cached->filepath);
    }

    /**
     * test cache->clear_all with cache_id and compile_id
     */
    public function testClearCacheAllCacheIdCompileId()
    {
        $this->smarty->clearAllCache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        // Custom CacheResources may return -1 if they can't tell the number of deleted elements
        $this->assertEquals(-1, $this->smarty->clearAllCache());
    }

    /**
     * test cache->clear with cache_id and compile_id
     */
    public function testClearCacheCacheIdCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world 1');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar2', 'blar');
        $tpl2->writeCachedContent('hello world 2');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world 3');
        // test cached content
        $this->assertEquals('hello world 1', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world 2', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world 3', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache(null, 'foo|bar'));
        // test that caches are deleted properly
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world 2', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheIdCompileId2()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar2', 'blar');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache('helloworld.tpl'));
        // test that caches are deleted properly
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheIdCompileId2Sub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar2', 'blar');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache('helloworld.tpl'));
        // test that caches are deleted properly
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheIdCompileId3()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheIdCompileId3Sub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheIdCompileId4()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheIdCompileId4Sub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheIdCompileId5()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache(null, null, 'blar'));
        // test that caches are deleted properly
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheIdCompileId5Sub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $tpl3->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache(null, null, 'blar'));
        // test that caches are deleted properly
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl3));
    }

    public function testClearCacheCacheFile()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', null, 'bar');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld.tpl', 'buh|blar');
        $tpl3->writeCachedContent('hello world');
        $tpl4 = $this->smarty->createTemplate('helloworld2.tpl');
        $tpl4->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl4));
        // test number of deleted caches
        $this->doClearCacheAssertion(3, $this->smarty->clearCache('helloworld.tpl'));
        // test that caches are deleted properly
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl2));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl3));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl4));
    }

    public function testClearCacheCacheFileSub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $tpl->writeCachedContent('hello world');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', null, 'bar');
        $tpl2->writeCachedContent('hello world');
        $tpl3 = $this->smarty->createTemplate('helloworld.tpl', 'buh|blar');
        $tpl3->writeCachedContent('hello world');
        $tpl4 = $this->smarty->createTemplate('helloworld2.tpl');
        $tpl4->writeCachedContent('hello world');
        // test cached content
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl2));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl3));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl4));
        // test number of deleted caches
        $this->doClearCacheAssertion(3, $this->smarty->clearCache('helloworld.tpl'));
        // test that caches are deleted properly
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl2));
        $this->assertNull($tpl->cached->handler->getCachedContent($tpl3));
        $this->assertEquals('hello world', $tpl->cached->handler->getCachedContent($tpl4));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedPrepare()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 1);
        // compile and cache
        $this->assertEquals('cache resource test:1 compiled:1 rendered:1', $this->smarty->fetch('cacheresource.tpl'));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCached()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 2);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertTrue($tpl->isCached());
        $this->assertEquals('cache resource test:2 compiled:1 rendered:1', $this->smarty->fetch($tpl));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedCachingDisabled()
    {
        $this->smarty->assign('test', 3);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached());
        $this->assertEquals('cache resource test:3 compiled:3 rendered:3', $this->smarty->fetch($tpl));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testForceCache()
    {
        $this->smarty->caching = true;
        $this->smarty->setForceCache(true);
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 4);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached(), 'isCached() must be false at forceCache');
        $this->assertEquals('cache resource test:4 compiled:1 rendered:4', $this->smarty->fetch($tpl));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedAftertestForceCache()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 5);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertTrue($tpl->isCached(), 'isCached() must be true after forceCache');
        $this->assertEquals('cache resource test:5 compiled:1 rendered:4', $this->smarty->fetch($tpl));
    }

    /**
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedTouchedSource()
    {
        sleep(2);
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 6);
        $filepath = $this->buildSourcePath(null, 'cacheresource.tpl', 'file');
        touch($filepath, $t = time());
        sleep(2);
        clearstatcache();
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertEquals($t,$tpl->source->timestamp);
        $isCached = $tpl->isCached();
        $mustCompile = $tpl->mustCompile();
        $this->assertEquals('cache resource test:6 compiled:6 rendered:6', $this->smarty->fetch($tpl), 'recompile failed');
        $this->assertFalse($isCached, 'isCached() must be false after touch of source');
        $this->assertTrue($mustCompile, 'mustCompile() must be true after touch of source');
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedAfterTouch()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 7);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertTrue($tpl->isCached(), 'isCached() must be true after recompilation');
        $this->assertEquals('cache resource test:7 compiled:6 rendered:6', $this->smarty->fetch($tpl));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedForceCompile()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setForceCompile(true);
        $this->smarty->assign('test', 8);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached(), 'isCached() must be false at force compile');
        $this->assertEquals('cache resource test:8 compiled:8 rendered:8', $this->smarty->fetch($tpl));
    }

    /**
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedAfterForceCompile()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 9);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertTrue($tpl->isCached(), 'isCached() must be true  after recompilation');
        $this->assertEquals('cache resource test:9 compiled:8 rendered:8', $this->smarty->fetch($tpl));
    }

    /**
     * Test
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedCacheNotLocked()
    {
        if ($this->smarty->caching_type != 'file') {
            $this->markTestSkipped('test skip for caching type not == file');
        }
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 10);
        $this->smarty->cache_locking = true;
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $start = time();
        $this->assertTrue($tpl->isCached(), 'isCached() must be true  after recompilation');
        $this->assertTrue((time() - $start) <= 2);
    }
    /**
     * Test
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testIsCachedCacheisLocked()
    {
        if ($this->smarty->caching_type != 'file') {
            $this->markTestSkipped('test skip for caching type not == file');
        }
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 11);
        $this->smarty->cache_locking = true;
        $this->smarty->locking_timeout = 4;
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $start = time();
        touch($tpl->cached->lock_id);
        $this->assertTrue($tpl->isCached(), 'isCached() must be true  after recompilation');
        $this->assertTrue((time() - $start) > 3);
        @unlink($tpl->cached->lock_id);
    }
    /**
     * Test
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testFetchCacheNotLocked()
    {
        if ($this->smarty->caching_type != 'file') {
            $this->markTestSkipped('test skip for caching type not == file');
        }
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 12);
        $this->smarty->cache_locking = true;
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $start = time();
        $this->assertEquals('cache resource test:12 compiled:8 rendered:8', $this->smarty->fetch($tpl));
        $this->assertTrue((time() - $start) <= 2);
    }
    /**
     * Test
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testFetchCacheisLocked()
    {
        if ($this->smarty->caching_type != 'file') {
            $this->markTestSkipped('test skip for caching type not == file');
        }
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', 13);
        $this->smarty->cache_locking = true;
        $this->smarty->locking_timeout = 4;
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $start = time();
        touch($tpl->cached->lock_id);
        $this->assertEquals('cache resource test:13 compiled:8 rendered:8', $this->smarty->fetch($tpl));
        $this->assertTrue((time() - $start) > 3);
        @unlink($tpl->cached->lock_id);
    }
}
