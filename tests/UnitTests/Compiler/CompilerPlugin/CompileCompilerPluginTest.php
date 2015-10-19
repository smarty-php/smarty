<?php
/**
 * Smarty PHPunit tests compilation of compiler plugins
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for compiler plugin tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileCompilerPluginTest extends PHPUnit_Smarty
{
     public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test compiler plugin tag in template file
     */
    public function testCompilerPlugin()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'compilerplugin', 'mycompilerplugin');
        $tpl = $this->smarty->createTemplate('compilerplugintest.tpl');
        $this->assertEquals("Hello World", $this->smarty->fetch($tpl));
    }
}

function mycompilerplugin($params, $compiler)
{
    return '<?php echo \'Hello World\';?>';
}
