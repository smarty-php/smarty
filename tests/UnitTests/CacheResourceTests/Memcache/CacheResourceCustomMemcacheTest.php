<?php
/**
 * Smarty PHPunit tests for cache resource memcache
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

include_once dirname(__FILE__) . '/../_shared/CacheResourceTestCommon.php';

/**
 * class for cache resource memcache tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CacheResourceCustomMemcacheTest extends CacheResourceTestCommon
{
    /**
     * Sets up the fixture
     * This method is called before a test is executed.
     *
     */
    public function setUp()
    {
        if (MemCacheEnable != true) {
            $this->markTestSkipped('Memcache tests are disabled');
        } else {
            if (!class_exists('Memcache')) {
                $this->markTestSkipped('Memcache not available');
            }
        }
        $this->setUpSmarty(dirname(__FILE__));
        parent::setUp();
        $this->smarty->setCachingType('memcachetest');
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    protected function doClearCacheAssertion($a, $b)
    {
        $this->assertEquals(- 1, $b);
    }

    public function testGetCachedFilepathSubDirs()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $tpl->loadCached();
        $sha1 = $tpl->source->uid . '#helloworld_tpl##';
        $this->assertEquals($sha1, $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with cache_id
     */
    public function testGetCachedFilepathCacheId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar');
        $tpl->loadCached();
        $sha1 = $tpl->source->uid . '#helloworld_tpl#foo|bar#';
        $this->assertEquals($sha1, $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with compile_id
     */
    public function testGetCachedFilepathCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', null, 'blar');
        $tpl->loadCached();
        $sha1 = $tpl->source->uid . '#helloworld_tpl##blar';
        $this->assertEquals($sha1, $tpl->cached->filepath);
    }

    /**
     * test getCachedFilepath with cache_id and compile_id
     */
    public function testGetCachedFilepathCacheIdCompileId()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('helloworld.tpl', 'foo|bar', 'blar');
        $tpl->loadCached();
        $sha1 = $tpl->source->uid . '#helloworld_tpl#foo|bar#blar';
        $this->assertEquals($sha1, $tpl->cached->filepath);
    }
}
