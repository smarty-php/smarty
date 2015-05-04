<?php
/**
 * Smarty PHPunit tests for cache resource file
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

include_once __DIR__ . '/../_shared/CacheResourceTestCommon.php';

/**
 * class for cache resource file tests
 *
 * @backupStaticAttributes enabled
 */
class CacheResourceCustomPDOTest extends CacheResourceTestCommon
{

    public function setUp($dir = null, $clear = true)
    {
        if (self::$config['cacheResource']['MysqlEnable'] != 'true') {
            $this->markTestSkipped('mysql tests are disabled');
        }
        if (self::$init) {
            $this->getConnection();
        }
        $this->setUpSmarty(__DIR__);
        parent::setUp();
        $this->smarty->setCachingType('pdo');
        $this->smarty->addPluginsDir(SMARTY_DIR . '../demo/plugins/');
        $this->assertTrue(false !== $this->smarty->loadPlugin('Smarty_CacheResource_Pdo'), 'loadPlugin() could not load PDO cache resource');
        $this->smarty->registerCacheResource('pdo', new Smarty_CacheResource_Pdo($this->getPDO(), 'output_cache'));
    }

    public function testInit()
    {
        $this->cleanDirs();
        $this->initMysqlCache();
    }
}
