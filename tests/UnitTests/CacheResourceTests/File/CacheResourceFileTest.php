<?php
/**
 * Smarty PHPunit tests for cache resource file
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

include_once dirname(__FILE__) . '/../_shared/CacheResourceTestCommon.php';

/**
 * class for cache resource file tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CacheResourceFileTest extends CacheResourceTestCommon
{

    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        parent::setUp();
        $this->smarty->setCachingType('filetest');
    }


    public function testInit()

    {
        $this->cleanDirs();
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
        $this->assertEquals($this->buildCachedPath($tpl, true, null, null, 'helloworld.tpl', $type = 'file', $this->smarty->getTemplateDir(0), 'file')
            , $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with cache_id
     */
    public function testGetCachedFilepathCacheId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar');
        $this->assertEquals($this->buildCachedPath($tpl, true, 'foo|bar', null, 'helloworld.tpl', $type = 'file', $this->smarty->getTemplateDir(0), 'file')
            , $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with compile_id
     */
    public function testGetCachedFilepathCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $tpl = $this->smarty->createTemplate('helloworld.tpl', null, 'blar');
        $this->assertEquals($this->buildCachedPath($tpl, true, null, 'blar', 'helloworld.tpl', $type = 'file', $this->smarty->getTemplateDir(0), 'file')
            , $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with cache_id and compile_id
     */
    public function testGetCachedFilepathCacheIdCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->assertEquals($this->buildCachedPath($tpl, true, 'foo|bar', 'blar', 'helloworld.tpl', $type = 'file', $this->smarty->getTemplateDir(0), 'file')
            , $tpl->cached->filepath);
    }

    /**
     * test cache->clear_all with cache_id and compile_id
     */
    public function testClearCacheAllCacheIdCompileId()
    {
        $this->cleanCacheDir();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertEquals(1, $this->smarty->clearAllCache());
    }

    /**
     * test cache->clear with cache_id and compile_id
     */
    public function testClearCacheCacheIdCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->cleanCacheDir();
        $this->smarty->setUseSubDirs(false);
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar2', 'blar');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(2, $this->smarty->clearCache(null, 'foo|bar'));
        $this->assertFalse(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertFalse(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheIdCompileId2()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(false);
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar2', 'blar');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(2, $this->smarty->clearCache('helloworld.tpl'));
        $this->assertFalse(file_exists($tpl->cached->filepath));
        $this->assertFalse(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheIdCompileId2Sub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar2', 'blar');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(2, $this->smarty->clearCache('helloworld.tpl'));
        $this->assertFalse(file_exists($tpl->cached->filepath));
        $this->assertFalse(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheIdCompileId3()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->cleanCacheDir();
        $this->smarty->setUseSubDirs(false);
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertFalse(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheIdCompileId3Sub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->cleanCacheDir();
        $this->smarty->setUseSubDirs(true);
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertFalse(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheIdCompileId4()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(false);
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertFalse(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheIdCompileId4Sub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(1, $this->smarty->clearCache('helloworld.tpl', null, 'blar2'));
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertFalse(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheIdCompileId5()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(false);
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(2, $this->smarty->clearCache(null, null, 'blar'));
        $this->assertFalse(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertFalse(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheIdCompileId5Sub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar2');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld2.tpl', 'foo|bar', 'blar');
        $this->writeCachedContent($tpl3);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertEquals(2, $this->smarty->clearCache(null, null, 'blar'));
        $this->assertFalse(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertFalse(file_exists($tpl3->cached->filepath));
    }

    public function testClearCacheCacheFile()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(false);
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', null, 'bar');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld.tpl', 'buh|blar');
        $this->writeCachedContent($tpl3);
        $tpl4 = $this->smarty->createTemplate('helloworld2.tpl');
        $this->writeCachedContent($tpl4);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertTrue(file_exists($tpl4->cached->filepath));
        $this->assertEquals(3, $this->smarty->clearCache('helloworld.tpl'));
        $this->assertFalse(file_exists($tpl->cached->filepath));
        $this->assertFalse(file_exists($tpl2->cached->filepath));
        $this->assertFalse(file_exists($tpl3->cached->filepath));
        $this->assertTrue(file_exists($tpl4->cached->filepath));
    }

    public function testClearCacheCacheFileSub()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->setUseSubDirs(true);
        $this->cleanCacheDir();
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->writeCachedContent($tpl);
        $tpl2 = $this->smarty->createTemplate('helloworld.tpl', null, 'bar');
        $this->writeCachedContent($tpl2);
        $tpl3 = $this->smarty->createTemplate('helloworld.tpl', 'buh|blar');
        $this->writeCachedContent($tpl3);
        $tpl4 = $this->smarty->createTemplate('helloworld2.tpl');
        $this->writeCachedContent($tpl4);
        $this->assertTrue(file_exists($tpl->cached->filepath));
        $this->assertTrue(file_exists($tpl2->cached->filepath));
        $this->assertTrue(file_exists($tpl3->cached->filepath));
        $this->assertTrue(file_exists($tpl4->cached->filepath));
        $this->assertEquals(3, $this->smarty->clearCache('helloworld.tpl'));
        $this->assertFalse(file_exists($tpl->cached->filepath));
        $this->assertFalse(file_exists($tpl2->cached->filepath));
        $this->assertFalse(file_exists($tpl3->cached->filepath));
        $this->assertTrue(file_exists($tpl4->cached->filepath));
    }

    /**
     * test cache->clear_all method
     */
    public function testClearCacheAll()
    {
        $this->cleanCacheDir();
        file_put_contents($this->smarty->getCacheDir() . 'dummy.php', 'test');
        $this->assertEquals(1, $this->smarty->clearAllCache());
    }

    /**
     * test cache->clear_all method not expired
     */
    public function testClearCacheAllNotExpired()
    {
        $this->cleanCacheDir();
        file_put_contents($this->smarty->getCacheDir() . 'dummy.php', 'test');
        touch($this->smarty->getCacheDir() . 'dummy.php', time() - 1000);
        clearstatcache();
        $this->assertEquals(0, $this->smarty->clearAllCache(2000));
    }

    /**
     * test cache->clear_all method expired
     */
    public function testClearCacheAllExpired()
    {
        $this->cleanCacheDir();
        file_put_contents($this->smarty->getCacheDir() . 'dummy.php', 'test');
        touch($this->smarty->getCacheDir() . 'dummy.php', time() - 1000);
        clearstatcache();
        $this->assertEquals(1, $this->smarty->clearAllCache(500));
    }


    private function writeCachedContent($tpl)
    {
        $tpl->writeCachedContent("echo 'hello world';\n");
    }
}
