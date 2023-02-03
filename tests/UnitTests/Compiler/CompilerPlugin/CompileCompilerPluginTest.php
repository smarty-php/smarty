<?php
/**
 * Smarty PHPunit tests compilation of compiler plugins
 *

 * @author  Uwe Tews
 */

/**
 * class for compiler plugin tests
 *
 *
 *
 *
 */
class CompileCompilerPluginTest extends PHPUnit_Smarty
{
     public function setUp(): void
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
    public function testCompilerPluginFunction()
    {
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_COMPILER, 'compilerplugin', 'mycompilerplugin');
        $this->smarty->setCompileId('function');
        $this->assertEquals("Hello World", $this->smarty->fetch('compilerplugintest.tpl'));
    }
    /**
     * test compiler plugin tag in template file
     */
    public function testCompilerPluginClassStatic()
    {
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_COMPILER, 'compilerplugin', array('CompilerPluginClass', 'statCompile'));
        $this->smarty->setCompileId('static');
        $this->assertEquals("Static World", $this->smarty->fetch('compilerplugintest.tpl'));
    }
    /**
     * test compiler plugin tag in template file
     */
    public function testCompilerPluginClassObject()
    {
        $plugin = new CompilerPluginClass;
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_COMPILER, 'compilerplugin', array($plugin, 'compile'));
        $this->smarty->setCompileId('object');
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