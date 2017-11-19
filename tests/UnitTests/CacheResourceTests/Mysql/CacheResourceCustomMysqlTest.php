<?php
/**
 * Smarty PHPunit tests for cache resource mysql
 *
 * @package PHPunit
 * @author  Uwe Tews
 */
if (MysqlCacheEnable == true) {

    include_once dirname(__FILE__) . '/../_shared/CacheResourceTestCommon.php';

    /**
     * class for cache resource file tests
     *
     * @runTestsInSeparateProcess
     * @preserveGlobalState    disabled
     * @backupStaticAttributes enabled
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
            $this->setUpSmarty(dirname(__FILE__));
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

