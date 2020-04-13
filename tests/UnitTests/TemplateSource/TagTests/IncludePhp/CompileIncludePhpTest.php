<?php
/**
 * Smarty PHPunit tests compilation of the {include_php} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {include_php} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileIncludePhpTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public $loadSmarty = false;
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smartyBC->setForceCompile(true);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test include_php string file_name function
     */
    public function testIncludePhpStringFileName()
    {
        $this->smartyBC->disableSecurity();
        $tpl = $this->smartyBC->createTemplate('include_php.tpl');
        $result = $this->smartyBC->fetch($tpl);
        $this->assertContains("test include php", $result);
    }

    /**
     * test include_php from trusted dir
     */
    public function testIncludePhpTrusted()
    {
        $this->smartyBC->enableSecurity();
        $this->smartyBC->security_policy->trusted_dir = '.\\scripts\\';
        $tpl = $this->smartyBC->createTemplate('include_php_trusted.tpl');
        $result = $this->smartyBC->fetch($tpl);
        $this->assertContains("test include php", $result);
    }

    /**
     * test include_php string file_name function
     */
    public function testIncludePhpVariableFileName()
    {
        $this->smartyBC->disableSecurity();
        $tpl = $this->smartyBC->createTemplate('string:start {include_php file=$filename once=false} end');
        $tpl->assign('filename', 'scripts/test_include_php.php');
        $result = $this->smartyBC->fetch($tpl);
        $this->assertContains("test include php", $result);
    }

    public function testIncludePhpVariableFileNameShortag()
    {
        $this->smartyBC->disableSecurity();
        $tpl = $this->smartyBC->createTemplate('string:start {include_php $filename once=false} end');
        $tpl->assign('filename', 'scripts/test_include_php.php');
        $result = $this->smartyBC->fetch($tpl);
        $this->assertContains("test include php", $result);
    }
}
