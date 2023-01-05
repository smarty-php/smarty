<?php
/**
 * Smarty PHPunit tests for cache resource registered
 *

 * @author  Uwe Tews
 */
if (MysqlCacheEnable == true) {
    include_once __DIR__ . '/../_shared/CacheResourceTestCommon.php';

    /**
     * class for cache resource file tests
     *
     * 
     * @preserveGlobalState    disabled
     * 
     */
    class CacheResourceCustomRegisteredTest extends CacheResourceTestCommon
    {
        public function setUp()
        {
            if (MysqlCacheEnable != true) {
                $this->markTestSkipped('mysql tests are disabled');
            }
            if (self::$init) {
                $this->getConnection();
            }
            $this->setUpSmarty(__DIR__);
            parent::setUp();
            if (!class_exists('Smarty_CacheResource_Mysqltest', false)) {
                require_once(__DIR__ . "/../_shared/PHPunitplugins/cacheresource.mysqltest.php");
            }
            $this->smarty->setCachingType('foobar');
            $this->smarty->registerCacheResource('foobar', new Smarty_CacheResource_Mysqltest());
        }

        public function testInit()
        {
            $this->cleanDirs();
            $this->initMysqlCache();
        }
    }
}

