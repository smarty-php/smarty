<?php

/**
 * Smarty PHPunit tests for File resources
 *
 * @package                PHPunit
 * @author                 Rodney Rehm
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class IncludePathTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->use_include_path = true;
        $this->smarty->setPluginsDir(array('./include','./include1'));
        $this->smarty->enableSecurity();
        $ds = DIRECTORY_SEPARATOR;
        set_include_path($this->smarty->_realpath(dirname(__FILE__) . "{$ds}..{$ds}..{$ds}..{$ds}Include_Path{$ds}Plugins{$ds}", true) . PATH_SEPARATOR . get_include_path());
    }

    /**
     * Tears down the fixture
     * This method is called after a test is executed.
     *
     */
    protected function tearDown()
    {
        ini_restore('include_path');
        $this->smarty->disableSecurity();
        parent::tearDown();
     }
    public function testInit()
    {
        $this->cleanDirs();
    }
    public function testInclude1()
    {
        $this->assertContains('plugin1', $this->smarty->fetch('test_include_path1.tpl'));
    }
    public function testInclude2()
    {
        $this->assertContains('plugin2', $this->smarty->fetch('test_include_path2.tpl'));
    }
    public function testInclude3()
    {
        $this->assertContains('plugin3', $this->smarty->fetch('test_include_path3.tpl'));
    }
  }
