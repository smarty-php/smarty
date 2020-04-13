<?php
/**
 * Smarty PHPunit tests for cache resource Pdo
 *
 * @package PHPunit
 * @author  Uwe Tews
 */
if (PdoCacheEnable == true) {

    include_once dirname(__FILE__) . '/../_shared/CacheResourceTestCommon.php';

    /**
     * class for cache resource file tests
     *
     * @runTestsInSeparateProcess
     * @preserveGlobalState    disabled
     * @backupStaticAttributes enabled
     */
    class CacheResourceCustomPDOTest extends CacheResourceTestCommon
    {

        public function setUp($dir = null, $clear = true)
        {
            if (PdoCacheEnable != true) {
                $this->markTestSkipped('mysql Pdo tests are disabled');
            }
            if (self::$init) {
                $this->getConnection();
            }
            $this->setUpSmarty(dirname(__FILE__));
            parent::setUp();
            $this->smarty->setCachingType('pdo');
            $this->smarty->addPluginsDir(SMARTY_DIR . '../demo/plugins/');
            $this->assertTrue(false !== $this->smarty->loadPlugin('Smarty_CacheResource_Pdotest'),
                              'loadPlugin() could not load PDO cache resource');
            $this->smarty->registerCacheResource('pdo',
                                                 new Smarty_CacheResource_Pdotest($this->getPDO(), 'output_cache'));
        }

        public function testInit()
        {
            $this->cleanDirs();
            $this->initMysqlCache();
        }
    }
}

