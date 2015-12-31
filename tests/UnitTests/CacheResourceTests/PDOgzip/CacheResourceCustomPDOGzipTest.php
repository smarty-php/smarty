<?php
/**
 * Smarty PHPunit tests for cache resource file
 *
 * @package PHPunit
 * @author  Uwe Tews
 */
if (PdoGzipCacheEnable == true) {

    include_once dirname(__FILE__) . '/../_shared/CacheResourceTestCommon.php';

    /**
     * class for cache resource file tests
     *
     * @backupStaticAttributes enabled
     */
    class CacheResourceCustomPDOGzipTest extends CacheResourceTestCommon
    {

        public function setUp($dir = null, $clear = true)
        {
            if (PdoGzipCacheEnable != true) {
                $this->markTestSkipped('mysql Pdo Gzip tests are disabled');
            }
            if (self::$init) {
                $this->getConnection();
            }
            $this->setUpSmarty(dirname(__FILE__));
            parent::setUp();
            $this->smarty->setCachingType('pdo');
            $this->smarty->addPluginsDir(SMARTY_DIR . '../demo/plugins/');
            $this->assertTrue(false !== $this->smarty->loadPlugin('Smarty_CacheResource_Pdo_Gziptest'),
                              'loadPlugin() could not load PDOGzip cache resource');
            $this->smarty->registerCacheResource('pdo', new Smarty_CacheResource_Pdo_Gziptest($this->getPDO(),
                                                                                              'output_cache'));
        }

        public function testInit()
        {
            $this->cleanDirs();
            $this->initMysqlCache();
        }
    }
}

