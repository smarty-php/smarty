<?php
/**
 * Smarty PHPunit tests register->modifier / unregister->modifier methods
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for register->modifier / unregister->modifier methods tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class RegisterModifierTest extends PHPUnit_Smarty
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
     * test register->modifier method for function
     */
    public function testRegisterModifier()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', 'mymodifier');
        $this->assertEquals('mymodifier', $this->smarty->registered_plugins[Smarty::PLUGIN_MODIFIER]['testmodifier'][0]);
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals('foo function blar bar', $this->smarty->fetch('eval:{$foo|testmodifier:blar:$bar}'));
    }

    /**
     * test register->modifier method for classes
     */
    public function testRegisterModifierClass()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', array('mymodifierclass', 'static_method'));
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals('foo static blar bar', $this->smarty->fetch('eval:{$foo|testmodifier:blar:$bar}'));
    }

    /**
     * test register->modifier method for objects
     */
    public function testRegisterModifierObject()
    {
        $obj = new mymodifierclass;
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', array($obj, 'object_method'));
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals('foo object blar bar', $this->smarty->fetch('eval:{$foo|testmodifier:blar:$bar}'));
    }

    /**
     * test unregister->modifier method
     */
    public function testUnregisterModifier()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', 'mymodifier');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_MODIFIER]['testmodifier']));
    }

    /**
     * test unregister->modifier method not registered
     */
    public function testUnregisterModifierNotRegistered()
    {
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier');
        $this->assertFalse(isset($this->smarty->registered_plugins[Smarty::PLUGIN_MODIFIER]['testmodifier']));
    }

    /**
     * test unregister->modifier method other registered
     */
    public function testUnregisterModifierOtherRegistered()
    {
        $this->smarty->registerPlugin(Smarty::PLUGIN_BLOCK, 'testmodifier', 'mymodifier');
        $this->smarty->unregisterPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier');
        $this->assertTrue(isset($this->smarty->registered_plugins[Smarty::PLUGIN_BLOCK]['testmodifier']));
    }

	public function testRegisterFirstClassCallable() {
		// skip test if PHP version is lower than 8.1
		if (PHP_VERSION_ID < 81000) {
			$this->markTestSkipped('PHP 8.1 or later is required');
		}

		$this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'testmodifier', strrev(...));
		$this->assertEquals('mosredna', $this->smarty->fetch('string:{"andersom"|testmodifier}'));
	}

	public function testRegisterFirstClassCallableSameName() {

		// skip test if PHP version is lower than 8.1
		if (PHP_VERSION_ID < 81000) {
			$this->markTestSkipped('PHP 8.1 or later is required');
		}

		$this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'mymodifier', mymodifier(...));
		$this->assertEquals('mosredna', $this->smarty->fetch('string:{"andersom"|mymodifier:"":""}'));
	}
	public function testRegisterFirstClassCallableAsFunc() {
		// skip test if PHP version is lower than 8.1
		if (PHP_VERSION_ID < 81000) {
			$this->markTestSkipped('PHP 8.1 or later is required');
		}

		$this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'kprint_r_out', strrev(...));
		$this->smarty->assign('myVar', 'andersom');
		$this->assertEquals('mosredna', $this->smarty->fetch('string:{kprint_r_out($myVar)}'));
	}

	/**
	 * @return void
	 * @throws SmartyException
	 * @group RegisterFirstClassCallableSameNameAsPhpFunc
	 */
	public function testRegisterFirstClassCallableSameNameAsPhpFunc() {
		// skip test if PHP version is lower than 8.1
		if (PHP_VERSION_ID < 81000) {
			$this->markTestSkipped('PHP 8.1 or later is required');
		}

		$this->smarty->registerPlugin(Smarty::PLUGIN_MODIFIER, 'mymodifier', strrev(...));
		$this->assertEquals('mosredna', $this->smarty->fetch('string:{mymodifier("andersom","","")}'));
	}

}

function mymodifier($a, $b, $c)
{
    return "$a function $b $c";
}

class mymodifierclass
{
    static function static_method($a, $b, $c)
    {
        return "$a static $b $c";
    }

    public function object_method($a, $b, $c)
    {
        return "$a object $b $c";
    }
}
