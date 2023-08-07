<?php
/**
 * Smarty PHPunit tests for cache resource file
 *

 * @author  Uwe Tews
 */

use Smarty\Template;

/**
 * class for cache resource file tests
 *
 * 
 */
abstract class CacheResourceTestCommon extends PHPUnit_Smarty
{
    public static $touchResource = null;

    public function setUp(): void
    {
        $this->smarty->setTemplateDir(__DIR__ . '/templates');
        $this->smarty->addPluginsDir(__DIR__ . '/PHPunitplugins');
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
    }


    protected function doClearCacheAssertion($a, $b)
    {
        $this->assertEquals($a, $b);
    }

    public function compiledPrefilter($text, Template $tpl)
    {
        $replace = $tpl->getTemplateVars('test');
        return str_replace('#', $replace ?? '', $text);
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
        $this->assertEquals($this->buildCachedPath($tpl), $tpl->getCached()->filepath);
    }

    /**
     * test getCachedFilepath with cache_id
     */
    public function testGetCachedFilepathCacheId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar');
        $this->assertEquals($this->buildCachedPath($tpl, true, 'foo|bar'), $tpl->getCached()->filepath);
    }

    /**
     * test getCachedFilepath with compile_id
     */
    public function testGetCachedFilepathCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', null, 'blar');
        $this->assertEquals($this->buildCachedPath($tpl, true, null, 'blar'), $tpl->getCached()->filepath);
    }

    /**
     * test getCachedFilepath with cache_id and compile_id
     */
    public function testGetCachedFilepathCacheIdCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->assertEquals($this->buildCachedPath($tpl, true, 'foo|bar', 'blar'), $tpl->getCached()->filepath);
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        // Custom CacheResources may return -1 if they can't tell the number of deleted elements
        //$this->assertEquals(-1, $this->smarty->clearAllCache());
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
        $this->assertEquals('hello world 1', $tpl->getCachedContent());
        $this->assertEquals('hello world 2', $tpl2->getCachedContent());
        $this->assertEquals('hello world 3', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache(null, 'foo|bar'));
        // test that caches are deleted properly
        $this->assertNull($tpl->getCachedContent());
        $this->assertEquals('hello world 2', $tpl2->getCachedContent());
        $this->assertNull($tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache('helloworld.tpl'));
        // test that caches are deleted properly
        $this->assertNull($tpl->getCachedContent());
        $this->assertNull($tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache('helloworld.tpl'));
        // test that caches are deleted properly
        $this->assertNull($tpl->getCachedContent());
        $this->assertNull($tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertNull($tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertNull($tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertNull($tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertNull($tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache(null, null, 'blar'));
        // test that caches are deleted properly
        $this->assertNull($tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertNull($tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(2, $this->smarty->clearCache(null, null, 'blar'));
        // test that caches are deleted properly
        $this->assertNull($tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertNull($tpl3->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        $this->assertEquals('hello world', $tpl4->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(3, $this->smarty->clearCache('helloworld.tpl'));
        // test that caches are deleted properly
        $this->assertNull($tpl->getCachedContent());
        $this->assertNull($tpl2->getCachedContent());
        $this->assertNull($tpl3->getCachedContent());
        $this->assertEquals('hello world', $tpl4->getCachedContent());
    }

    /**
     * @group slow
     */
   public function testClearCacheExpired()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->clearAllCache();
        // create and cache templates
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $tpl->writeCachedContent('something else 1');
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', null, 'bar');
        $tpl2->writeCachedContent('something else 2');
        $tpl3 = $this->smarty->createTemplate('helloworld.tpl', 'buh|blar');
        $tpl3->writeCachedContent('something else 3');
        // test cached content
        $this->assertEquals('something else 1', $tpl->getCachedContent());
        $this->assertEquals('something else 2', $tpl2->getCachedContent());
        $this->assertEquals('something else 3', $tpl3->getCachedContent());
        sleep(4);
        $tpl4 = $this->smarty->createTemplate('helloworld2.tpl');
        $tpl4->writeCachedContent('something else 4');

        // test number of deleted caches
        $this->doClearCacheAssertion(3,$this->smarty->clearAllCache(2));

	    $tpl = $this->smarty->createTemplate('helloworld.tpl');
	    $tpl2 = $this->smarty->createTemplate('helloworld.tpl', null, 'bar');
	    $tpl3 = $this->smarty->createTemplate('helloworld.tpl', 'buh|blar');
	    $tpl4 = $this->smarty->createTemplate('helloworld2.tpl');

        // test that caches are deleted properly
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        $this->assertEquals('something else 4', $tpl4->getCachedContent());
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
        $this->assertEquals('hello world', $tpl->getCachedContent());
        $this->assertEquals('hello world', $tpl2->getCachedContent());
        $this->assertEquals('hello world', $tpl3->getCachedContent());
        $this->assertEquals('hello world', $tpl4->getCachedContent());
        // test number of deleted caches
        $this->doClearCacheAssertion(3, $this->smarty->clearCache('helloworld.tpl'));
        // test that caches are deleted properly
        $this->assertNull($tpl->getCachedContent());
        $this->assertNull($tpl2->getCachedContent());
        $this->assertNull($tpl3->getCachedContent());
        $this->assertEquals('hello world', $tpl4->getCachedContent());
    }
    /**
     * Test caching
     * @dataProvider data
     * @group slow
     */
    public function testCache($lockTime, $lockTimeout, $compile_id, $cache_id, $isCached, $tmin, $tmax, $forceCompile, $forceCache, $update, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('test', $testNumber);
        if ($lockTimeout) {
            $this->smarty->cache_locking = true;
            $this->smarty->locking_timeout = $lockTimeout;
        }
        $this->smarty->setCompileId($compile_id);
        $this->smarty->cache_id = $cache_id;
        $this->smarty->force_compile = $forceCompile;
        $this->smarty->force_cache = $forceCache;
        if ($update) {
            sleep(1);
            $filepath = $this->buildSourcePath(null, 'cacheresource.tpl', 'file');
            touch($filepath, $t = time());
             clearstatcache();
        }
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        if ($update) {
            $this->assertEquals($t,$tpl->getSource()->getTimeStamp(), $testName . ' - source touch');
        }
        if ($lockTime) {
            $tpl->getCached()->handler->acquireLock($this->smarty, $tpl->getCached());
            $tpl->getCached()->handler->lockTime = $lockTime;
            $tpl->getCached()->is_locked = false;
        }
        $start = time();
        if (isset($isCached)) {
            $this->assertEquals($isCached, $tpl->isCached(), $testName . ' - isCached()');
            if ($lockTimeout) {
                $time = time() - $start;
                $this->assertTrue($time >= $tmin && $time <= $tmax, $testName . ' - isCached() - lock time');
                $this->assertEquals(!$isCached, $tpl->getCached()->handler->hasLock($this->smarty, $tpl->getCached()), $testName . ' - isCached() - lock status');
            } else {
                $this->assertFalse($tpl->getCached()->handler->hasLock($this->smarty, $tpl->getCached()), $testName . ' - isCached() - unexpected lock');
            }
        }
        $this->assertEquals("cache resource test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $this->smarty->fetch($tpl), $testName . ' - fetch() failure');
        if ($lockTime) {
            $time = time() - $start;
            $this->assertTrue($time >= $tmin && $time <= $tmax, $testName . ' - fetch() - lock time');
        }
        $this->assertFalse($tpl->getCached()->handler->hasLock($this->smarty, $tpl->getCached(), $testName . ' - lock not removed'));
    }

    public function data(){
        return array(
            /*
             * lock time
             * locking_timeout 0 = no cache_locking
             * compile_id
             * cache_id
             * isCached()  expected result (null = no isCached test)
             * min elapsed time
             * max elapsed time
             * force compile
             * force cache
             * source update
             * test nr
             * result compile nr
             * result render nr
             * text
             */
            array(0, 0, null, null, false, 0, 0, false, false, false, 1, 1, 1, 'locking off - create cache'),
            array(0, 0, 1, null, false, 0, 0, false, false, false, 2, 2, 2, 'locking off - create cache compile_id'),
            array(0, 0, null, 2, false, 0, 0, false, false, false, 3, 1, 3, 'locking off - create cache cache_id'),
            array(0, 0, 3, 4, false, 0, 0, false, false, false, 4, 4, 4, 'locking off - create cache cache_id & compile_id'),
            array(0, 0, null, null, null, 0, 0, false, false, false, 5, 1, 1, 'locking off - fetch'),
            array(0, 0, 1, null, null, 0, 0, false, false, false, 6, 2, 2, 'locking off - fetch compile_id'),
            array(0, 0, null, 2, null, 0, 0, false, false, false, 7, 1, 3, 'locking off - fetch cache_id'),
            array(0, 0, 3, 4, null, 0, 0, false, false, false, 8, 4, 4, 'locking off - fetch cache_id & compile_id'),
            array(0, 0, null, null, true,0, 0, false, false, false, 9, 1, 1, 'locking off - isCached & fetch'),
            array(0, 0, 1, null, true, 0, 0, false, false, false, 10, 2, 2, 'locking off - isCached & fetch compile_id'),
            array(0, 0, null, 2, true, 0, 0, false, false, false, 11, 1, 3, 'locking off - isCached & fetch cache_id'),
            array(0, 0, 3, 4, true, 0, 0, false, false, false, 12, 4, 4, 'locking off - isCached & fetch cache_id & compile_id'),
            array(0, 0, null, null, false, 0, 0, true, false, false, 13, 13, 13, 'locking off - force compile'),
            array(0, 0, null, null, true, 0, 0, false, false, false, 14, 13, 13, 'locking off - after force compile'),
            array(0, 0, null, null, false, 0, 0, false, true, false, 15, 13, 15, 'locking off - force cache'),
            array(0, 0, null, null, true, 0, 0, false, false, false, 16, 13, 15, 'locking off - after force cache'),
            array(0, 0, null, null, false, 0, 0, false, false, true, 17, 17, 17, 'locking off - new source'),
            array(0, 0, null, null, true, 0, 0, false, false, false, 18, 17, 17, 'locking off - after new source'),
            array(0, 5, null, null, null, 0, 1, false, false, false, 19, 17, 17, 'not locked - fetch'),
            array(0, 5, null, null, true, 0, 1, false, false, false, 20, 17, 17, 'not locked - isCached & fetch'),
            array(0, 5, null, null, false, 0, 1, false, false, true, 21, 21, 21, 'not locked - new source'),
            array(0, 5, null, null, true, 0, 1, false, false, false, 22, 21, 21, 'not locked - after new source'),
            array(4, 10, null, null, null, 2, 10, false, false, false, 23, 21, 21, 'locked - fetch'),
            array(4, 10, null, null, true, 2, 10, false, false, false, 24, 21, 21, 'locked - isCached & fetch'),
            array(4, 10, null, null, false, 2, 10, false, false, true, 25, 25, 25, 'locked - new source'),
            array(4, 10, null, null, true, 2, 10, false, false, false, 26, 25, 25, 'locked - after new source'),
            array(10, 4, null, null, null, 2, 10, false, false, false, 27, 25, 25, 'lock timeout - fetch'),
            array(10, 4, null, null, true, 2, 10, false, false, false, 28, 25, 25, 'lock timeout - isCached & fetch'),
            array(10, 4, null, null, false, 2, 10, false, false, true, 29, 29, 29, 'lock timeout - new source'),
            array(10, 4, null, null, true, 2, 10, false, false, false, 30, 29, 29, 'lock timeout - after new source'),
            array(4, 10, 5, null, false, 2, 10, false, false, false, 31, 31, 31, 'locked - new compiled'),
            array(10, 4, 6, null, false, 2, 10, false, false, false, 32, 32, 32, 'lock timeout - new compiled'),
        );
    }

    public function testCachingDisabled1()
    {
        $this->smarty->assign('test', 50);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached(), 'isCached() status');
        $this->assertEquals('cache resource test:50 compiled:50 rendered:50', $this->smarty->fetch($tpl), 'fetch()');
    }
    public function testCachingDisabled2()
    {
        $this->smarty->assign('test', 51);
        $tpl = $this->smarty->createTemplate('cacheresource.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached(), 'isCached() status');
        $this->assertEquals('cache resource test:51 compiled:50 rendered:51', $this->smarty->fetch($tpl), 'fetch()');
    }

    /**
     * Test caching
     * @dataProvider dataDir
     *
     */
    public function testCachingTemplateDir($folder, $iscached, $merge, $result)
    {
        $this->smarty->setCaching(true);
        if ($folder == 0) {
            $this->smarty->setTemplateDir(array(__DIR__ . '/../_shared/templates', __DIR__ . '/../_shared/templates/a'));
        } else {
            $this->smarty->setTemplateDir(array(__DIR__ . '/../_shared/templates', __DIR__ . '/../_shared/templates/b'));
        }
        if ($merge) {
            $this->smarty->setCompileId(1);
            $this->smarty->setMergeCompiledIncludes(true);
        }
        $tpl = $this->smarty->createTemplate('templatedir.tpl', $this->smarty);
        $this->assertEquals($iscached, $tpl->isCached());
        $this->assertStringContainsString($result, $tpl->fetch());
    }

    public function dataDir(){
        return array(
            array(0,false,0, 'include a'),
            array(0,true,0, 'include a'),
            array(1,false,0, 'include b'),
            array(1,true,0, 'include b'),
            array(0,false,1, 'include a'),
            array(0,true,1, 'include a'),
            array(1,false,1, 'include b'),
            array(1,true,1, 'include b'),
            );
    }
}
