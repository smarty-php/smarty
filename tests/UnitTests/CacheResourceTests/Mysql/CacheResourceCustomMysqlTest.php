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
class CacheResourceCustomMysqlTest extends CacheResourceTestCommon
{

    public function setUp()
    {
        if (self::$config['cacheResource']['MysqlEnable'] != 'true') {
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
