<?php
/**
 * Smarty PHPunit tests for cache resource Pdo
 *

 * @author  Uwe Tews
 */

include_once __DIR__ . '/../_shared/CacheResourceTestCommon.php';
include_once __DIR__ . '/cacheresource.pdotest.php';

/**
 * class for cache resource file tests
 *
 * 
 * @preserveGlobalState    disabled
 * 
 */
class CacheResourceCustomPDOTest extends CacheResourceTestCommon
{

    public function setUp($dir = null, $clear = true): void
    {
        if (PdoCacheEnable != true) {
            $this->markTestSkipped('mysql Pdo tests are disabled');
        }
        if (self::$init) {
            $this->getConnection();
        }
        $this->setUpSmarty(__DIR__);
        parent::setUp();
        $this->smarty->registerCacheResource('pdo',
                                             new Smarty_CacheResource_Pdotest($this->getPDO(), 'output_cache'));
    }

    public function testInit()
    {
        $this->cleanDirs();
        $this->initMysqlCache();
    }
}

