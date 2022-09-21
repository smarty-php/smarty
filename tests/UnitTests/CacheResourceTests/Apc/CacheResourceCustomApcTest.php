<?php
/**
 * Smarty PHPunit tests for cache resource Apc
 *
 * @package PHPunit
 * @author  Uwe Tews
 */
include_once dirname(__FILE__) . '/../Memcache/CacheResourceCustomMemcacheTest.php';

/**
 * class for cache resource file tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CacheResourceCustomApcTest extends CacheResourceCustomMemcacheTest
{
    public function setUp(): void
    {
        if (!function_exists('apc_cache_info') || ini_get('apc.enable_cli')) {
            $this->markTestSkipped('APC cache not available');
        }
        $this->setUpSmarty(dirname(__FILE__));
        parent::setUp();
        $this->smarty->setCachingType('apc');
        $this->smarty->addPluginsDir(SMARTY_DIR . '../demo/plugins/');
    }
}

