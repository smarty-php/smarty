<?php
/**
 * Smarty PHPunit tests of the {config_load} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for config variable tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileConfigLoadTest extends PHPUnit_Smarty
{
    /**
     * Sets up the fixture
     * This method is called before a test is executed.
     *
     */
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("../../../__shared/templates/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    /**
     * empty template_c and cache folders
     */
    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     * test {load_config} loading section2
     */
    public function testConfigVariableSection2Template_001()
    {
        $this->assertEquals("Welcome to Smarty! Global Section1 Hello Section2", $this->smarty->fetch('001_section2.tpl'));
    }
    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     * test {load_config} loading section2
     */
    public function testConfigVariableSection2Template_0012()
    {
        $this->smarty->caching = true;
        $this->assertEquals("Welcome to Smarty! Global Section1 Hello Section2", $this->smarty->fetch('001_section2.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     * test {load_config} loading section2
     */
    public function testConfigVariableInclude_003()
    {
        $this->assertEquals("Welcome to Smarty! Global Section1 Hello Section2", $this->smarty->fetch('003_section2.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     * test {load_config} loading section2 shorttags
     */
    public function testConfigVariableSection2TemplateShorttags()
    {
        $this->assertEquals("Welcome to Smarty! Global Section1 Hello Section2", $this->smarty->fetch('002_section2.tpl'));
    }

    /**
     * test config varibales loading all sections from template
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableAllSectionsTemplate_004()
    {
        $this->smarty->setConfigOverwrite(true);
        $this->assertEquals("Welcome to Smarty! Global Section1 Global Section2", $this->smarty->fetch('004_allsection.tpl'));
    }

    /**
     * test config varibales overwrite
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwrite_005()
    {
        $this->assertEquals("Overwrite2", $this->smarty->fetch('005_overwrite.tpl'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwrite_006()
    {
        $this->assertEquals("Welcome to Smarty! Overwrite3", $this->smarty->fetch('006_overwrite.tpl'));
    }
    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwrite_0061()
    {
        $this->smarty->configLoad('test.conf', 'default');
        $this->smarty->configLoad('test2.conf', 'default');
        $this->assertEquals('Welcome to overwrite test! this overwitten', $this->smarty->fetch('0061_overwrite.tpl'));
    }

    /**
     * test config varibales overwrite false
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwrite_007()
    {
        $this->smarty->setConfigOverwrite(false);
        $this->assertEquals("Overwrite1 Overwrite2 Overwrite3 ", $this->smarty->fetch('007_overwrite.tpl'));
    }

    /**
     * test config varibales booleanize on
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableBooleanizeOn_008()
    {
        $this->smarty->setConfigBooleanize(true);
        $this->smarty->assign('expected', true);
        $this->assertEquals("passed", $this->smarty->fetch('008_booleanize.tpl'));
    }

    /**
     * test config varibales booleanize off
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableBooleanizeOff_008()
    {
        $this->smarty->setConfigBooleanize(false);
        $this->smarty->assign('expected', 'on');
        $this->assertEquals("passed", $this->smarty->fetch('008_booleanize.tpl'));
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in config file
     * test config file syntax error
     */
    public function testConfigSyntaxError_009()
    {
        $this->smarty->fetch('009_error.tpl');
    }
}
