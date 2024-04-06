<?php
/**
 * Smarty PHPunit tests assignByRef method
 */

/**
 * Class for assignByRef tests
 */
class AssignByRefTest extends PHPUnit_Smarty
{
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}

	public function testInit()
	{
		$this->cleanDirs();
	}

	private $testStr = null;
	/**
	 * Test assignByRef for nullable string property
	 */
	public function testAssignByRefForNullableStringProperry()
	{
		$this->smarty->assignByRef('myVar', $this->testStr);
		$this->assertEquals(null, $this->smarty->fetch('eval:{$myVar}'));
		$this->testStr = 'abc';
		$this->assertEquals('abc', $this->smarty->fetch('eval:{$myVar}'));
	}

	/**
	 * Test assignByRef for string
	 */
	public function testAssignByRefForString()
	{
		$var = 'abc';
		$this->smarty->assignByRef('myVar', $var);
		$this->assertEquals('abc', $this->smarty->fetch('eval:{$myVar}'));
		$var = 'def';
		$this->assertEquals('def', $this->smarty->fetch('eval:{$myVar}'));
	}

	/**
	 * Test assignByRef for array
	 */
	public function testAssignByRefForArray()
	{
		$var = array(
			'a' => 'A',
		);
		$this->smarty->assignByRef('myVar', $var);
		$this->assertEquals('{"a":"A"}', $this->smarty->fetch('eval:{$myVar|json_encode}'));
		$var['b'] = 'B';
		$this->assertEquals('{"a":"A","b":"B"}', $this->smarty->fetch('eval:{$myVar|json_encode}'));
	}

	/**
	 * Test that assignByRef returns this.
	 */
	public function testAssignByRefReturnsThis()
	{
		$var = 'data';
		$this->assertEquals(
			'data',
			$this->smarty->assignByRef('dummy', $var)->fetch('eval:{$dummy}')
		);
	}
}
