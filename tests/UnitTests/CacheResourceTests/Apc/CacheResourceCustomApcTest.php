<?php
/**
 * Smarty PHPunit tests for cache resource Apc
 *

 * @author  Uwe Tews
 */
include_once __DIR__ . '/../Memcache/CacheResourceCustomMemcacheTest.php';
include_once __DIR__ . '/../_shared/PHPunitplugins/cacheresource.apctest.php';

/**
 * class for cache resource file tests
 *
 * 
 * @preserveGlobalState    disabled
 *
 */
class CacheResourceCustomApcTest extends CacheResourceCustomMemcacheTest
{
    public function setUp(): void
    {
        if (!function_exists('apc_cache_info') || ini_get('apc.enable_cli')) {
            $this->markTestSkipped('APC cache not available');
        }
        $this->setUpSmarty(__DIR__);
        parent::setUp();
        $this->smarty->setCachingType('apc');
        $this->smarty->registerCacheResource('apc', new Smarty_CacheResource_Apctest());
    }
}

