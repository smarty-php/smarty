<?php
/**
 * Smarty PHPunit tests of config  variables
 *

 * @author  Uwe Tews
 */

/**
 * class for config variable tests
 *
 * 
 * 
 *
 */
class ConfigVarTest extends PHPUnit_Smarty
{
    /**
     * Sets up the fixture
     * This method is called before a test is executed.
     *
     */
    public function setUp(): void
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
     * test number config variable
     */
    public function testConfigNumber()
    {
        $this->smarty->configLoad('test.conf');
        $this->assertEquals("123.4", $this->smarty->fetch('number.tpl'));
    }

    /**
     * test string config variable
     */
    public function testConfigText()
    {
        $this->smarty->configLoad('test.conf');
        $this->assertEquals("123bvc", $this->smarty->fetch('text.tpl'));
    }

    /**
     * test line string config variable
     */
    public function testConfigLine()
    {
        $this->smarty->configLoad('test.conf');
        $this->assertEquals("123 This is a line", $this->smarty->fetch('eval:{#line#}'));
    }

    /**
     * test config variables in global sections
     */
    public function testConfigVariableGlobalSections()
    {
        $this->smarty->configLoad('test.conf');
        $this->assertEquals("Welcome to Smarty! Global Section1 Global Section2", $this->smarty->fetch('sec1sec2.tpl'));
    }

    /**
     * test config variables loading section2
     */
    public function testConfigVariableSection2()
    {
        $this->smarty->configLoad('test.conf', 'section2');
        $this->assertEquals("Welcome to Smarty! Global Section1 Hello Section2", $this->smarty->fetch('sec1sec2.tpl'));
    }

    /**
     * test config variables loading section special char
     */
    public function testConfigVariableSectionSpecialChar()
    {
        $this->smarty->configLoad('test.conf', '/');
        $this->assertEquals("Welcome to Smarty! special char", $this->smarty->fetch('sec.tpl'));
    }

    /**
     * test config variables loading section foo/bar
     */
    public function testConfigVariableSectionFooBar()
    {
        $this->smarty->configLoad('test.conf', 'foo/bar');
        $this->assertEquals("Welcome to Smarty! section foo/bar", $this->smarty->fetch('sec.tpl'));
    }

    /**
     * test config variables loaded in different scopes from different sections (Smarty and template)
     */
    public function testConfigDifferentScope()
    {
        $this->smarty->configLoad('test.conf', 'section2');
        $tpl = $this->smarty->createTemplate('sec1sec2.tpl');
        $tpl->configLoad('test.conf', 'section1');
        $this->assertEquals("Welcome to Smarty! Global Section1 Hello Section2", $this->smarty->fetch('sec1sec2.tpl'));
        $this->assertEquals("Welcome to Smarty! Hello Section1 Global Section2", $this->smarty->fetch($tpl));
    }

    /**
     * test config variables of hidden sections
     * shall display variables from hidden section
     */
    public function testConfigVariableHidden()
    {
        $this->smarty->config_read_hidden = true;
        $this->smarty->configLoad('test.conf', 'hidden');
        $this->assertEquals("Welcome to Smarty!Hidden Section", $this->smarty->fetch('hidden.tpl'));
    }

    /**
     * test config variables of disabled hidden sections
     * shall display not variables from hidden section
     */
    public function testConfigVariableHiddenDisable()
    {
        $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
        $this->smarty->config_read_hidden = false;
        $this->smarty->configLoad('test.conf', 'hidden');
        $this->assertEquals("Welcome to Smarty!", $this->smarty->fetch('hidden.tpl'));
    }

    /**
     * test getConfigVars
     */
    public function testConfigGetSingleConfigVar()
    {
        $this->smarty->configLoad('test.conf');
        $this->assertEquals("Welcome to Smarty!", $this->smarty->getConfigVars('title'));
    }

    /**
     * test getConfigVars return all variables
     */
    public function testConfigGetAllConfigVars()
    {
        $this->smarty->configLoad('test.conf');
        $vars = $this->smarty->getConfigVars();
        $this->assertTrue(is_array($vars));
        $this->assertEquals("Welcome to Smarty!", $vars['title']);
        $this->assertEquals("Global Section1", $vars['sec1']);
    }

    /**
     * test clearConfig for single variable
     */
    public function testConfigClearSingleConfigVar()
    {
        $this->smarty->configLoad('test.conf');
        $this->smarty->clearConfig('title');
        $this->assertEquals("", $this->smarty->getConfigVars('title'));
    }

    /**
     * test clearConfig for all variables
     */
    public function testConfigClearConfigAll()
    {
        $this->smarty->configLoad('test.conf');
        $this->smarty->clearConfig();
        $vars = $this->smarty->getConfigVars();
        $this->assertTrue(is_array($vars));
        $this->assertTrue(empty($vars));
    }

    /**
     * test config vars on data object
     */
    public function testConfigTextData()
    {
        $data = $this->smarty->createData();
        $data->configLoad('test.conf');
        $tpl = $this->smarty->createTemplate('text.tpl', $data);
        $this->assertEquals("123bvc", $this->smarty->fetch($tpl));
    }

    /**
     * test getConfigVars on data object
     */
    public function testConfigGetSingleConfigVarData()
    {
        $data = $this->smarty->createData();
        $data->configLoad('test.conf');
        $this->assertEquals("Welcome to Smarty!", $data->getConfigVars('title'));
    }

    /**
     * test getConfigVars return all variables on data object
     */
    public function testConfigGetAllConfigVarsData()
    {
        $data = $this->smarty->createData();
        $data->configLoad('test.conf');
        $vars = $data->getConfigVars();
        $this->assertTrue(is_array($vars));
        $this->assertEquals("Welcome to Smarty!", $vars['title']);
        $this->assertEquals("Global Section1", $vars['sec1']);
    }

    /**
     * test clearConfig for single variable on data object
     */
    public function testConfigClearSingleConfigVarData()
    {
        $data = $this->smarty->createData();
        $data->configLoad('test.conf');
        $data->clearConfig('title');
        $this->assertEquals("", $data->getConfigVars('title'));
        $this->assertEquals("Global Section1", $data->getConfigVars('sec1'));
    }

    /**
     * test clearConfig for all variables on data object
     */
    public function testConfigClearConfigAllData()
    {
        $data = $this->smarty->createData();
        $data->configLoad('test.conf');
        $data->clearConfig();
        $vars = $data->getConfigVars();
        $this->assertTrue(is_array($vars));
        $this->assertTrue(empty($vars));
    }

    /**
     * test config vars on template object
     */
    public function testConfigTextTemplate()
    {
        $tpl = $this->smarty->createTemplate('text.tpl');
        $tpl->configLoad('test.conf');
        $this->assertEquals("123bvc", $this->smarty->fetch($tpl));
    }

    /**
     * test getConfigVars on template object
     */
    public function testConfigGetSingleConfigVarTemplate()
    {
        $tpl = $this->smarty->createTemplate('text.tpl');
        $tpl->configLoad('test.conf');
        $this->assertEquals("Welcome to Smarty!", $tpl->getConfigVars('title'));
    }

    /**
     * test getConfigVariable on template object
     */
    public function testConfigGetSingleConfigVarTemplate2()
    {
        $tpl = $this->smarty->createTemplate('text.tpl');
        $tpl->configLoad('test.conf');
        $this->assertEquals("Welcome to Smarty!", $tpl->getConfigVariable('title'));
    }

    /**
     * test getConfigVars return all variables on template object
     */
    public function testConfigGetAllConfigVarsTemplate()
    {
        $tpl = $this->smarty->createTemplate('text.tpl');
        $tpl->configLoad('test.conf');
        $vars = $tpl->getConfigVars();
        $this->assertTrue(is_array($vars));
        $this->assertEquals("Welcome to Smarty!", $vars['title']);
        $this->assertEquals("Global Section1", $vars['sec1']);
    }

    /**
     * test clearConfig for single variable on template object
     */
    public function testConfigClearSingleConfigVarTemplate()
    {
        $tpl = $this->smarty->createTemplate('text.tpl');
        $tpl->configLoad('test.conf');
        $tpl->clearConfig('title');
        $this->assertEquals("", $tpl->getConfigVars('title'));
        $this->assertEquals("Global Section1", $tpl->getConfigVars('sec1'));
    }

    /**
     * test clearConfig for all variables on template object
     */
    public function testConfigClearConfigAllTemplate()
    {
        $tpl = $this->smarty->createTemplate('text.tpl');
        $tpl->configLoad('test.conf');
        $tpl->clearConfig();
        $vars = $tpl->getConfigVars();
        $this->assertTrue(is_array($vars));
        $this->assertTrue(empty($vars));
    }

    /**
     * test config variables loading from absolute file path
     */
    public function testConfigAbsolutePath()
    {
        $file = realpath($this->smarty->getConfigDir(0) . 'test.conf');
        $this->smarty->configLoad($file);
        $this->assertEquals("123.4", $this->smarty->fetch('number.tpl'));
    }

    public function testConfigResourceDb4()
    {
        $this->smarty->addPluginsDir(__DIR__ . "/../../ResourceTests/ResourcePlugins/PHPunitplugins/");
        $this->smarty->configLoad('db4:foo.conf');
        $this->assertEquals("bar", $this->smarty->fetch('foo.tpl'));
    }
    public function testConfigUndefinedSilent()
    {
        $this->assertEquals("", $this->smarty->fetch('foo.tpl'));
    }

    public function testConfigUndefinedNotice()
    {
        $this->smarty->error_unassigned = true;
        try {
            $this->assertEquals("", $this->smarty->fetch('foo.tpl'));
        }
        catch (Exception $e) {
            $this->assertStringStartsWith('Undefined variable', $e->getMessage());
        }
    }
}
