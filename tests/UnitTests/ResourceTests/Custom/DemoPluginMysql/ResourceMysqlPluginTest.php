<?php
/**
 * Smarty PHPunit tests resource plugins
 *

 * @author  Uwe Tews
 */
if (MysqlResourceEnable == true) {
    /**
     * class for resource plugins tests
     *
     * 
     * 
     * 
     */
    class ResourceMysqlPluginTest extends PHPUnit_Smarty
    {
        public function setUp(): void
        {
            if (MysqlResourceEnable != true) {
                $this->markTestSkipped('Msqlresource tests are disabled');
            }
                if (self::$init) {
                $this->getConnection();
            }
            $this->setUpSmarty(__DIR__);
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
        */
        public function testResourcePluginMysql()
        {
             $this->assertEquals('hello world', $this->smarty->fetch('mysqltest:test.tpl'));
        }

       /**
        * test must compile
        */
        public function testMustCompile()
        {
           $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
           $this->assertFalse($tpl->mustCompile());
        }

        /**
        * test must compile
        * @group slow
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
        */
        public function testResourcePluginMysql2()
        {
             $this->assertEquals('hello smarty', $this->smarty->fetch('mysqltest:test.tpl'));
        }

         /**
        * test clear compiled
        */
        public function testClearCompiled()
        {
            $this->assertEquals(1, $this->smarty->clearCompiledTemplate('mysqltest:test.tpl'));
        }

       /**
        * test must compile
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
            $this->assertEquals($this->buildCompiledPath($tpl, false, false, null, 'test.tpl', 'mysqltest', $this->smarty->getTemplateDir(0)), $tpl->getCompiled()->filepath);
        }

        public function testResourcePluginMysqlCompiledFilepathCache()
        {
            $this->smarty->caching = true;
            $this->smarty->cache_lifetime = 1000;
            $this->smarty->setForceCompile(true);
            $this->smarty->fetch('mysqltest:test.tpl');
            $tpl = $this->smarty->createTemplate('mysqltest:test.tpl');
            $this->assertEquals($this->buildCompiledPath($tpl, false, true, null, 'test.tpl', 'mysqltest', $this->smarty->getTemplateDir(0)), $tpl->getCompiled()->filepath);
            $this->smarty->caching = false;
        }

        /**
        * test unknown template
        */
        public function testUnknownTemplate() {
            $this->expectException(\Smarty\Exception::class);
            $this->expectExceptionMessage('Unable to load \'mysqlstest:foo.tpl\'');
            $this->assertEquals('foo', $this->smarty->fetch('mysqlstest:foo.tpl'));
        }
    }
}
