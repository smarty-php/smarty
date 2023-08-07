<?php
/**
 * Smarty PHPunit tests for cache resource mysql
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
    class CacheResourceCustomMysqlTest extends CacheResourceTestCommon
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
            $this->smarty->setCachingType('mysqltest');
        }

        public function testInit()
        {
            $this->cleanDirs();
            $this->initMysqlCache();
        }
    }
}

