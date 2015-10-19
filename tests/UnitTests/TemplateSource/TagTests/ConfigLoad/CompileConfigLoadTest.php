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
 * @preserveGlobalState disabled
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
        $this->setUpSmarty(__DIR__);
    }

    /**
     * empty templat_c and cache folders
     */
    public function testInit()
    {
        $this->cleanDirs();
    }

     /**
      * test {load_config} loading section2
     */
    public function testConfigVariableSection2Template()
    {
        $this->assertEquals("Welcome to Smarty! Global Section1 Hello Section2", $this->smarty->fetch('eval:{config_load file=\'test.conf\' section=\'section2\'}{#title#} {#sec1#} {#sec2#}'));
    }

    /**
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     *
     * test {load_config} loading section2 shorttags
     */
    public function testConfigVariableSection2TemplateShorttags()
    {
        $this->assertEquals("Welcome to Smarty! Global Section1 Hello Section2", $this->smarty->fetch('eval:{config_load \'test.conf\' \'section2\'}{#title#} {#sec1#} {#sec2#}'));
    }

    /**
     * test config varibales loading local
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableLocal()
    {
        $this->assertEquals("Welcome to Smarty!", $this->smarty->fetch('eval:{config_load file=\'test.conf\' scope=\'local\'}{#title#}'));
        // global must be empty
        $this->assertEquals("", $this->smarty->getConfigVars('title'));
    }

    /**
     * test config varibales loading parent
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableParent()
    {
        $this->assertEquals("Welcome to Smarty!", $this->smarty->fetch('eval:{config_load file=\'test.conf\' scope=\'parent\'}{#title#}'));
        // global is parent must not be empty
        $this->assertEquals("Welcome to Smarty!", $this->smarty->getConfigVars('title'));
    }

    /**
     * test config varibales loading global
     *
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableGlobal()
    {
        $this->assertEquals("Welcome to Smarty!", $this->smarty->fetch('eval:{config_load file=\'test.conf\' scope=\'global\'}{#title#}'));
        // global is parent must not be empty
        $this->assertEquals("Welcome to Smarty!", $this->smarty->getConfigVars('title'));
    }


    /**
     * test config varibales loading all sections from template
     *
     * @run InSeparateProcess
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
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwrite()
    {
        $this->assertEquals("Overwrite2", $this->smarty->fetch('eval:{config_load file=\'test.conf\'}{#overwrite#}'));
    }

    /**
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testConfigVariableOverwrite2()
    {
        $this->assertEquals("Overwrite3", $this->smarty->fetch('eval:{config_load file=\'test.conf\'}{config_load file=\'test2.conf\'}{#overwrite#}'));
    }

    /**
     * test config varibales overwrite false
     *
     * @run InSeparateProcess
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
     * @run InSeparateProcess
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
     * @run InSeparateProcess
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
}
