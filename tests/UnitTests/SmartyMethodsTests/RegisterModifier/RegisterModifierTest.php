<?php
/**
 * Smarty PHPunit tests register->modifier / unregister->modifier methods
 *

 * @author  Uwe Tews
 */

/**
 * class for register->modifier / unregister->modifier methods tests
 *
 * 
 * 
 * 
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
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier', 'mymodifier');
        $this->assertEquals('mymodifier', $this->smarty->getRegisteredPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier')[0]);
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals('foo function blar bar', $this->smarty->fetch('eval:{$foo|testmodifier:blar:$bar}'));
    }

    /**
     * test register->modifier method for classes
     */
    public function testRegisterModifierClass()
    {
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier', array('mymodifierclass', 'static_method'));
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
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier', array($obj, 'object_method'));
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals('foo object blar bar', $this->smarty->fetch('eval:{$foo|testmodifier:blar:$bar}'));
    }

    /**
     * test unregister->modifier method
     */
    public function testUnregisterModifier()
    {
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier', 'mymodifier');
        $this->smarty->unregisterPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier');
        $this->assertNull($this->smarty->getRegisteredPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier'));
    }

    /**
     * test unregister->modifier method not registered
     */
    public function testUnregisterModifierNotRegistered()
    {
        $this->smarty->unregisterPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier');
	    $this->assertNull($this->smarty->getRegisteredPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier'));
    }

    /**
     * test unregister->modifier method other registered
     */
    public function testUnregisterModifierOtherRegistered()
    {
        $this->smarty->registerPlugin(\Smarty\Smarty::PLUGIN_BLOCK, 'testmodifier', 'mymodifier');
        $this->smarty->unregisterPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, 'testmodifier');
	    $this->assertIsArray($this->smarty->getRegisteredPlugin(\Smarty\Smarty::PLUGIN_BLOCK, 'testmodifier'));
    }

	/**
	 * test cannot call native PHP fuctions by default
	 * @dataProvider dataUnknownModifiers
	 */
	public function testNativePHPModifiers($template, $expectedValue)
	{
		$this->cleanDirs();
		$this->expectException(\Smarty\CompilerException::class);
		$this->expectExceptionMessage('unknown modifier');
		$this->smarty->fetch('string:' . $template);
	}

	public function dataUnknownModifiers(): array {
		return [
			['{" blah"|ltrim:" "}', 'blah'],
			['{"blah"|strrev}', 'halb'],
			['{"blah"|ucfirst}', 'Blah'],
			['{"blah"|md5}', md5('blah')],
		];
	}

	/**
	 * test register wildcard modifier using extension
	 * @dataProvider dataUnknownModifiers
	 */
	public function testUnregisterModifiers($template, $expectedValue)
	{
		$this->cleanDirs();
		$this->smarty->addExtension(new WildcardExtension());
		$this->assertEquals($expectedValue, $this->smarty->fetch('string:' . $template));
	}

	/**
	 * test register wildcard modifier using setExtensions
	 * @dataProvider dataUnknownModifiers
	 */
	public function testSetExtensions($template, $expectedValue)
	{
		$this->cleanDirs();
		$this->smarty->setExtensions([
			new \Smarty\Extension\CoreExtension(),
			new WildcardExtension()
		]);
		$this->assertEquals($expectedValue, $this->smarty->fetch('string:' . $template));
	}

}

class WildcardExtension extends \Smarty\Extension\Base {

	public function getModifierCallback(string $modifierName) {
		if (is_callable($modifierName)) {
			return $modifierName;
		}
		return null;
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
