<?php
/**
 * Smarty PHPunit tests resource plugins
 *
 * @package PHPunit
 * @author  Uwe Tews
 */
if (defined(MysqlResourceEnable) && MysqlResourceEnable == 'true') {
    /**
     * class for resource plugins tests
     *
     * @runTestsInSeparateProcess
     * @preserveGlobalState disabled
     * @backupStaticAttributes enabled
     */
    class ResourceMysqlPluginTest extends PHPUnit_Smarty
    {
        public function setUp()
        {
            if (self::$init) {
                $this->getConnection();
            }
            $this->setUpSmarty(__DIR__);
        }

        public function testInit()
        {
            $this->cleanDirs();
            $this->initMysqlResource();
            PHPUnit_Smarty::$pdo->exec("REPLACE INTO templates VALUES ('test.tpl', '2010-12-25 22:00:00', '{\$x = \'hello world\'}{\$x}' )");
        }

        /**
         * test resource plugin rendering of a custom resource
         */
        public function testResourcePluginMysql()
        {
            //$this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
            $this->smarty->addPluginsDir("./PHPunitplugins/");
            $this->assertEquals('hello world', $this->smarty->fetch('mysqltest:test.tpl'));
        }

        /**
         * test resource plugin timestamp of a custom resource
         */
        public function testResourcePluginMysqlTimestamp()
        {
            //       $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
            $this->smarty->addPluginsDir("./PHPunitplugins/");
            $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
            $this->assertEquals(strtotime("2010-12-25 22:00:00"), $tpl->source->getTimeStamp());
        }

        /**
         * test resource plugin compiledFilepath of a custom resource
         */
        public function testResourcePluginMysqlCompiledFilepath()
        {
            //       $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
            $this->smarty->addPluginsDir("./PHPunitplugins/");
            $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
            $this->assertEquals($this->buildCompiledPath($tpl, false, false, null, 'test.tpl', 'mysqltest', $this->smarty->getTemplateDir(0)), $tpl->compiled->filepath);
        }

        public function testResourcePluginMysqlCompiledFilepathCache()
        {
            //$this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
            $this->smarty->addPluginsDir("./PHPunitplugins/");
            $this->smarty->caching = true;
            $this->smarty->cache_lifetime = 1000;
            $this->smarty->setForceCompile(true);
            $this->smarty->fetch('mysqltest:test.tpl');
            $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
            $this->assertEquals($this->buildCompiledPath($tpl, false, true, null, 'test.tpl', 'mysqltest', $this->smarty->getTemplateDir(0)), $tpl->compiled->filepath);
            $this->smarty->caching = false;
        }

        /**
         * test resource plugin timestamp of a custom resource with only fetch() implemented
         */
        public function testResourcePluginMysqlTimestampWithoutFetchTimestamp()
        {
            //       $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
            $this->smarty->addPluginsDir("./PHPunitplugins/");
            $tpl = $this->smarty->createTemplate('mysqlstest:test.tpl');
            $this->assertEquals(strtotime("2010-12-25 22:00:00"), $tpl->source->getTimeStamp());
        }
    }
}
