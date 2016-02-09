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
    public function testConfigVariableAllSectionsTemplate()
    {
        $this->smarty->setConfigOverwrite(true);
        $this->assertEquals("Welcome to Smarty! Global Section1 Global Section2", $this->smarty->fetch('eval:{config_load file=\'test.conf\'}{#title#} {#sec1#} {#sec2#}'));
    }

    /**
     * test config varibales overwrite
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwrite()
    {
        $this->assertEquals("Overwrite2", $this->smarty->fetch('eval:{config_load file=\'test.conf\'}{#overwrite#}'));
    }

    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwrite2()
    {
        $this->assertEquals("Overwrite3", $this->smarty->fetch('eval:{config_load file=\'test.conf\'}{config_load file=\'test2.conf\'}{#overwrite#}'));
    }

    /**
     * test config varibales overwrite false
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwriteFalse()
    {
        $this->smarty->setConfigOverwrite(false);
        $this->assertEquals("Overwrite1Overwrite2Overwrite3Welcome to Smarty! Global Section1 Global Section2", $this->smarty->fetch('eval:{config_load file=\'test.conf\'}{config_load file=\'test2.conf\'}{foreach #overwrite# as $over}{$over}{/foreach}{#title#} {#sec1#} {#sec2#}'));
    }

    /**
     * test config varibales booleanize on
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableBooleanizeOn()
    {
        $this->smarty->setConfigBooleanize(true);
        $this->assertEquals("passed", $this->smarty->fetch('eval:{config_load file=\'test.conf\'}{if #booleanon# === true}passed{/if}'));
    }

    /**
     * test config varibales booleanize off
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableBooleanizeOff()
    {
        $this->smarty->setConfigBooleanize(false);
        $this->assertEquals("passed", $this->smarty->fetch('eval:{config_load file=\'test.conf\'}{if #booleanon# == \'on\'}passed{/if}'));
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in config file
     * test config file syntax error
     */
    public function testConfigSyntaxError()
    {
        $this->smarty->fetch('eval:{config_load file=\'test_error.conf\'}');
    }

    /**
     * Test scope
     *
     * @not                 runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestScope
     */
    public function testScope($code, $useSmarty, $result, $testName, $testNumber)
    {
        $file = "testScope_{$testNumber}.tpl";
        $this->makeTemplateFile($file, $code . '{checkconfigvar var=foo}');
        $this->smarty->assignGlobal('file', $file);
        $this->smarty->configLoad('smarty.conf');
        $data = $this->smarty->createData($useSmarty ? $this->smarty : null);
        $data->configLoad('data.conf');
        $this->assertEquals($this->strip('#' . $file . $result), $this->strip($this->smarty->fetch('scope_tag.tpl', $data)), "test - {$code} - {$testName}");
    }

    /*
     * Data provider für testscope
     */
    public function dataTestScope()
    {
        $i = 1;
        /*
         * Code
         * use Smarty object
         * result
         * test name
         */
        return array(
            array(
                '{config_load \'template.conf\'}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=local}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=local bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=parent}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=parent bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=tpl_root}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=tpl_root bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=root}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=root bubble_up}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=root}', false,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'',
                'no smarty', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=root bubble_up}', false,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'newvar\'#Smarty:$foo=\'smarty\'',
                'no  smarty', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=smarty}', true,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'data\'#scope_tag.tpl:$foo=\'data\'#data:$foo=\'data\'#Smarty:$foo=\'newvar\'',
                '', $i ++,
            ), array(
                '{config_load \'template.conf\' scope=smarty bubble_up}', false,
                ':$foo=\'newvar\'#scope_include.tpl:$foo=\'newvar\'#scope_tag.tpl:$foo=\'newvar\'#data:$foo=\'data\'#Smarty:$foo=\'newvar\'',
                'no  smarty', $i ++,
            ),
        );
    }
}
