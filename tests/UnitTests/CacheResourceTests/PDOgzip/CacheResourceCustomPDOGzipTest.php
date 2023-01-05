<?php
/**
 * Smarty PHPunit tests for cache resource file
 *

 * @author  Uwe Tews
 */


include_once __DIR__ . '/../_shared/CacheResourceTestCommon.php';
include_once __DIR__ . '/cacheresource.pdo_gziptest.php';

/**
 * class for cache resource file tests
 *
 * 
 */
class CacheResourceCustomPDOGzipTest extends CacheResourceTestCommon
{

    public function setUp($dir = null, $clear = true): void
    {
        if (PdoGzipCacheEnable != true) {
            $this->markTestSkipped('mysql Pdo Gzip tests are disabled');
        }
        if (self::$init) {
            $this->getConnection();
        }
        $this->setUpSmarty(__DIR__);
        parent::setUp();
        $this->smarty->setCachingType('pdo');
        $this->smarty->registerCacheResource('pdo', new Smarty_CacheResource_Pdo_Gziptest($this->getPDO(),
                                                                                          'output_cache'));
    }

    public function testInit()
    {
        $this->cleanDirs();
        $this->initMysqlCache();
    }
}

