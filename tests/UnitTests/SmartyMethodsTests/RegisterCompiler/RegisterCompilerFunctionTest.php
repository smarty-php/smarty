<?php
/**
 * Smarty PHPunit tests register->compilerFunction / unregister->compilerFunction methods
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for register->compilerFunction / unregister->compilerFunction methods tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class RegisterCompilerFunctionTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test register->compilerFunction method for function
     */
    public function testRegisterCompilerFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'testcompilerfunction', 'mycompilerfunction');
        $this->assertEquals('mycompilerfunction', $this->smarty->registered_plugins['compiler']['testcompilerfunction'][0]);
        $this->assertEquals('hello world 1', $this->smarty->fetch('eval:{testcompilerfunction var=1}'));
    }

    /**
     * test register->compilerFunction method for blocks
     */
    public function testRegisterCompilerFunctionBlock()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'foo', 'mycompilerfunctionopen');
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'fooclose', 'mycompilerfunctionclose');
        $result = $this->smarty->fetch('eval:{foo} hallo {/foo}');
        $this->assertEquals('open tag hallo close tag', $result);
    }

    /**
     * test register->compilerFunction method for static class
     */
    public function testRegisterCompilerFunctionClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'testcompilerfunction', array('mycompilerfunctionclass', 'execute'));
        $this->assertEquals('hello world 2', $this->smarty->fetch('eval:{testcompilerfunction var1=2}'));
    }

    /**
     * test register->compilerFunction method for objects
     */
    public function testRegisterCompilerFunctionObject()
    {
        $obj = new mycompilerfunctionclass;
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'testcompilerfunction', array($obj, 'compile'));
        $this->assertEquals('hello world 3', $this->smarty->fetch('eval:{testcompilerfunction var2=3}'));
    }

    /**
     * test unregister->compilerFunction method
     */
    public function testUnregisterCompilerFunction()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_COMPILER, 'testcompilerfunction', 'mycompilerfunction');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_COMPILER, 'testcompilerfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_COMPILER]['testcompilerfunction']));
    }

    /**
     * test unregister->compilerFunction method not registered
     */
    public function testUnregisterCompilerFunctionNotRegistered()
    {
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_COMPILER, 'testcompilerfunction');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_COMPILER]['testcompilerfunction']));
    }

    /**
     * test unregister->compilerFunction method other registered
     */
    public function testUnregisterCompilerFunctionOtherRegistered()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testcompilerfunction', 'mycompilerfunction');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_COMPILER, 'testcompilerfunction');
        $this->assertTrue(isset($this->smarty->registered_plugins[Smarty::PLUGIN_BLOCK]['testcompilerfunction']));
    }
}

function mycompilerfunction($params, $smarty)
{
    return "<?php echo 'hello world {$params['var']}';?>";
}

function mycompilerfunctionopen($params, $smarty)
{
    return "<?php echo 'open tag';?>";
}

function mycompilerfunctionclose($params, $smarty)
{
    return "<?php echo 'close tag';?>";
}

class mycompilerfunctionclass
{
    static function execute($params, $smarty)
    {
        return "<?php echo 'hello world {$params['var1']}';?>";
    }

    public function compile($params, $smarty)
    {
        return "<?php echo 'hello world {$params['var2']}';?>";
    }
}
