<?php
/**
 * Smarty PHPunit tests for cache resource registered
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
            $this->setUpSmarty(dirname(__FILE__));
            parent::setUp();
            if (!class_exists('Smarty_CacheResource_Mysqltest', false)) {
                require_once(dirname(__FILE__) . "/../_shared/PHPunitplugins/cacheresource.mysqltest.php");
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

