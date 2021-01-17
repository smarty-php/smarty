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
     public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test compiler plugin tag in template file
     */
    public function testCompilerPluginFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'compilerplugin', 'mycompilerplugin');
        $this->smarty->compile_id = 'function';
        $this->assertEquals("Hello World", $this->smarty->fetch('compilerplugintest.tpl'));
    }
    /**
     * test compiler plugin tag in template file
     */
    public function testCompilerPluginClassStatic()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'compilerplugin', array('CompilerPluginClass', 'statCompile'));
        $this->smarty->compile_id = 'static';
        $this->assertEquals("Static World", $this->smarty->fetch('compilerplugintest.tpl'));
    }
    /**
     * test compiler plugin tag in template file
     */
    public function testCompilerPluginClassObject()
    {
        $plugin = new CompilerPluginClass;
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'compilerplugin', array($plugin, 'compile'));
        $this->smarty->compile_id = 'object';
        $this->assertEquals("Public World", $this->smarty->fetch('compilerplugintest.tpl'));
    }
}

function mycompilerplugin($params, $compiler)
{
    return '<?php echo \'Hello World\';?>';
}

class CompilerPluginClass
{
    static function statCompile ($params, $compiler) {
        return '<?php echo \'Static World\';?>';
    }
    public function compile ($params, $compiler) {
        return '<?php echo \'Public World\';?>';
    }
}