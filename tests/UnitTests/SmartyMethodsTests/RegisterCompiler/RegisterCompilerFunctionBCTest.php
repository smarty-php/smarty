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
class RegisterCompilerFunctionBCTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public $loadSmarty = false;

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
        $this->smartyBC->register_compiler_function('testcompilerfunction', 'mycompilerfunctionBC');
        $this->assertEquals('mycompilerfunctionBC', $this->smartyBC->registered_plugins['compiler']['testcompilerfunction'][0]);
        $this->assertEquals('hello world 1', $this->smartyBC->fetch('eval:{testcompilerfunction var=1}'));
    }

    /**
     * test register->compilerFunction method for blocks
     */
    public function testRegisterCompilerFunctionBlock()
    {
        $this->smartyBC->register_compiler_function('foo', 'mycompilerfunctionBCopen');
        $this->smartyBC->register_compiler_function('fooclose', 'mycompilerfunctionBCclose');
        $result = $this->smartyBC->fetch('eval:{foo} hallo {/foo}');
        $this->assertEquals('open tag hallo close tag', $result);
    }

    /**
     * test register->compilerFunction method for static class
     */
    public function testRegisterCompilerFunctionClass()
    {
        $this->smartyBC->register_compiler_function('testcompilerfunction', array('mycompilerfunctionBCclass', 'execute'));
        $this->assertEquals('hello world 2', $this->smartyBC->fetch('eval:{testcompilerfunction var1=2}'));
    }

    /**
     * test register->compilerFunction method for objects
     */
    public function testRegisterCompilerFunctionObject()
    {
        $obj = new mycompilerfunctionBCclass;
        $this->smartyBC->register_compiler_function('testcompilerfunction', array($obj, 'compile'));
        $this->assertEquals('hello world 3', $this->smartyBC->fetch('eval:{testcompilerfunction var2=3}'));
    }

    /**
     * test unregister->compilerFunction method
     */
    public function testUnregisterCompilerFunction()
    {
        $this->smartyBC->register_compiler_function('testcompilerfunction', 'mycompilerfunctionBC');
        $this->smartyBC->unregister_compiler_function('testcompilerfunction');
        $this->assertFalse(isset($this->smartyBC->registered_plugins[Smarty::PLUGIN_COMPILER]['testcompilerfunction']));
    }

    /**
     * test unregister->compilerFunction method not registered
     */
    public function testUnregisterCompilerFunctionNotRegistered()
    {
        $this->smartyBC->unregister_compiler_function('testcompilerfunction');
        $this->assertFalse(isset($this->smartyBC->registered_plugins[Smarty::PLUGIN_COMPILER]['testcompilerfunction']));
    }

    /**
     * test unregister->compilerFunction method other registered
     */
    public function testUnregisterCompilerFunctionOtherRegistered()
    {
        $this->smartyBC->register_block('testcompilerfunction', 'mycompilerfunctionBC');
        $this->smartyBC->unregister_compiler_function('testcompilerfunction');
        $this->assertTrue(isset($this->smartyBC->registered_plugins[Smarty::PLUGIN_BLOCK]['testcompilerfunction']));
    }
}

function mycompilerfunctionBC($params, $smarty)
{
    return "<?php echo 'hello world {$params['var']}';?>";
}

function mycompilerfunctionBCopen($params, $smarty)
{
    return "<?php echo 'open tag';?>";
}

function mycompilerfunctionBCclose($params, $smarty)
{
    return "<?php echo 'close tag';?>";
}

class mycompilerfunctionBCclass
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
