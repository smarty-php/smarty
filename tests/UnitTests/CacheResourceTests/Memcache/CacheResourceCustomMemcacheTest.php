<?php
/**
 * Smarty PHPunit tests for cache resource memcache
 *
 * @package PHPunit
 * @author  Uwe Tews
 */
if (MemCacheEnable == true) {

    include_once __DIR__ . '/../_shared/CacheResourceTestCommon.php';

    /**
     * class for cache resource memcache tests
     *
     * @runTestsInSeparateProcess
     * @preserveGlobalState    disabled
     * @backupStaticAttributes enabled
     */
    class CacheResourceCustomMemcacheTest extends CacheResourceTestCommon
    {
        /**
         * Sets up the fixture
         * This method is called before a test is executed.
         *
         */
        public function setUp()
        {
            if (MemCacheEnable != true) {
                $this->markTestSkipped('Memcache tests are disabled');
            }
            $this->setUpSmarty(__DIR__);
            parent::setUp();
            $this->smarty->setCachingType('memcachetest');
        }

        public function testInit()
        {
            $this->cleanDirs();
        }

        protected function doClearCacheAssertion($a, $b)
        {
            $this->assertEquals(- 1, $b);
        }
    }
}
