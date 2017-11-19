<?php
/**
 * Smarty PHPunit tests resource plugins
 *
 * @package PHPunit
 * @author  Uwe Tews
 */
if (MysqlResourceEnable == true) {
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
            if (MysqlResourceEnable != true) {
                $this->markTestSkipped('Msqlresource tests are disabled');
            }
                if (self::$init) {
                $this->getConnection();
            }
            $this->setUpSmarty(dirname(__FILE__));
            $this->smarty->addPluginsDir("./PHPunitplugins/");
        }

        /**
        *
        */
        public function testInit()
        {
            $this->cleanDirs();
            $this->initMysqlResource();
            PHPUnit_Smarty::$pdo->exec("REPLACE INTO templates (name, source) VALUES ('test.tpl', '{\$x = \'hello world\'}{\$x}')");
        }

        /**
        * test resource plugin rendering of a custom resource
        *
        * @runInSeparateProcess
        * @preserveGlobalState disabled
        *
        */
        public function testResourcePluginMysql()
        {
             $this->assertEquals('hello world', $this->smarty->fetch('mysqltest:test.tpl'));
        }

       /**
        * test must compile
        *
        * @runInSeparateProcess
        * @preserveGlobalState disabled
        *
        */
        public function testMustCompile()
        {
           $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
           $this->assertFalse($tpl->mustCompile());
        }

         /**
        * test must compile
        *
        * @runInSeparateProcess
        * @preserveGlobalState disabled
        *
        */
        public function testMustCompile2()
        {
            sleep(2);
            PHPUnit_Smarty::$pdo->exec("REPLACE INTO templates (name, source) VALUES ('test.tpl',  '{\$x = \'hello smarty\'}{\$x}' )");
            $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
            $this->assertTrue($tpl->mustCompile());
        }

        /**
        * test resource plugin rendering of a custom resource
        *
        * @runInSeparateProcess
        * @preserveGlobalState disabled
        *
        */
        public function testResourcePluginMysql2()
        {
             $this->assertEquals('hello smarty', $this->smarty->fetch('mysqltest:test.tpl'));
        }

         /**
        * test clear compiled
        *
        * @runInSeparateProcess
        * @preserveGlobalState disabled
        *
        */
        public function testClearCompiled()
        {
            $this->assertEquals(1, $this->smarty->clearCompiledTemplate('mysqltest:test.tpl'));
        }

       /**
        * test must compile
        *
        * @runInSeparateProcess
        * @preserveGlobalState disabled
        *
        */
        public function testMustCompile3()
        {
           $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
           $this->assertTrue($tpl->mustCompile());
        }

        /**
        * test resource plugin compiledFilepath of a custom resource
        */
        public function testResourcePluginMysqlCompiledFilepath()
        {
            //$this->smarty->addPluginsDir("./PHPunitplugins/");
            $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
            $this->assertEquals($this->buildCompiledPath($tpl, false, false, null, 'test.tpl', 'mysqltest', $this->smarty->getTemplateDir(0)), $tpl->compiled->filepath);
        }

        public function testResourcePluginMysqlCompiledFilepathCache()
        {
            $this->smarty->caching = true;
            $this->smarty->cache_lifetime = 1000;
            $this->smarty->setForceCompile(true);
            $this->smarty->fetch('mysqltest:test.tpl');
            $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
            $this->assertEquals($this->buildCompiledPath($tpl, false, true, null, 'test.tpl', 'mysqltest', $this->smarty->getTemplateDir(0)), $tpl->compiled->filepath);
            $this->smarty->caching = false;
        }

        /**
        * test unknown template
        *
        * @expectedException        SmartyException
        * @expectedExceptionMessage Unable to load template 'mysqlstest:foo.tpl'
        * @runInSeparateProcess
        * @preserveGlobalState disabled
        *
        */
        public function testUnknownTemplate() {
            $this->assertEquals('foo', $this->smarty->fetch('mysqlstest:foo.tpl'));
        }
    }
}
