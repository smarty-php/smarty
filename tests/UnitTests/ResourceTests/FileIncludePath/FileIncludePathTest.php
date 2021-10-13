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
class FileIncludePathTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->use_include_path = true;
        $this->smarty->setTemplateDir(array('./templates', './templates_2', './include'));
        $this->smarty->enableSecurity();
        $ds = DIRECTORY_SEPARATOR;
        set_include_path($this->smarty->_realpath(dirname(__FILE__) . "{$ds}..{$ds}..{$ds}..{$ds}Include_Path{$ds}Tpl{$ds}", true) . PATH_SEPARATOR . get_include_path());
    }

    /**
     * Tears down the fixture
     * This method is called after a test is executed.
     *
     */
    protected function tearDown(): void
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
        $this->assertStringContainsString('include_test1', $this->smarty->fetch('test1.tpl'));
    }
    public function testInclude2()
    {
        $this->assertStringContainsString('include_test2', $this->smarty->fetch('test2.tpl'));
    }
    public function testInclude3()
    {
        $this->assertStringContainsString('include_test3', $this->smarty->fetch('test3.tpl'));
    }
    public function testInclude31()
    {
        $this->smarty->use_include_path = false;
        $this->smarty->security_policy->secure_dir = getcwd();
        $this->assertStringContainsString('include_test3', $this->smarty->fetch('test3.tpl'));
    }
    public function testInclude4()
    {
        $this->assertStringContainsString('include_test4', $this->smarty->fetch('test4.tpl'));
    }
    public function testInclude5()
    {
        $this->smarty->setTemplateDir(array('./'));
        $this->assertStringContainsString('include path root', $this->smarty->fetch('test5.tpl'));
    }

 }
