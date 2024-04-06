<?php
/**
 * Smarty PHPunit tests assignByRef method
 */

use Smarty\Data;

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
	public function testAssignByRefForNullableStringProperty()
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

	/**
	 * Test duplicate assignByRef
	 */
	public function testDuplicateAssignByRef()
	{
		$var1 = array(
			'a' => 'A',
		);
		$var2= array(
			'z' => 'Z',
		);
		$this->smarty->assignByRef('myVar', $var1);
		$this->smarty->assignByRef('myVar', $var2);
		$this->assertEquals('{"z":"Z"}', $this->smarty->fetch('eval:{$myVar|json_encode}'));
		$var1['b'] = 'B';
		$var2['y'] = 'Y';
		$this->assertEquals('{"z":"Z","y":"Y"}', $this->smarty->fetch('eval:{$myVar|json_encode}'));
	}

	/**
	 * Test assignByRef for parent scope
	 */
	public function testAssignByRefForParentScope()
	{
		$data1 = $this->smarty->createData($this->smarty);
		$data2 = $this->smarty->createData($data1);
		$var1 = array(
			'a1' => 'A1'
		);
		$var2 = array(
			'b1' => 'B1'
		);
		$var3 = array(
			'c1' => 'C1'
		);
		$data1->assignByRef('var1', $var1);
		$data2->assignByRef('var2', $var2, false, Data::SCOPE_PARENT);
		$data2->assignByRef('var3', $var3);
		$this->assertEquals($var1, $data2->getTemplateVars('var1'));
		$this->assertEquals(null, $data2->getTemplateVars('var1', false));
		$this->assertEquals($var2, $data2->getTemplateVars('var2'));
		$this->assertEquals(null, $data2->getTemplateVars('var2', false));
		$this->assertEquals($var3, $data2->getTemplateVars('var3'));
		$this->assertEquals($var3, $data2->getTemplateVars('var3', false));

		$var1['a2'] = 'A2';
		$var2['b2'] = 'B2';
		$var3['c2'] = 'C2';
		$this->assertEquals($var1, $data2->getTemplateVars('var1'));
		$this->assertEquals(null, $data2->getTemplateVars('var1', false));
		$this->assertEquals($var2, $data2->getTemplateVars('var2'));
		$this->assertEquals(null, $data2->getTemplateVars('var2', false));
		$this->assertEquals($var3, $data2->getTemplateVars('var3'));
		$this->assertEquals($var3, $data2->getTemplateVars('var3', false));
	}

	/**
	 * Test assignByRef for root scope
	 */
	public function testAssignByRefForRootScope()
	{
		$data1 = $this->smarty->createData($this->smarty);
		$data2 = $this->smarty->createData($data1);
		$data3 = $this->smarty->createData($data2);
		$var1 = array(
			'a1' => 'A1'
		);
		$var2 = array(
			'b1' => 'B1'
		);
		$var3 = array(
			'c1' => 'C1'
		);
		$data1->assignByRef('var1', $var1);
		$data2->assignByRef('var2', $var2, false, Data::SCOPE_PARENT);
		$data3->assignByRef('var3', $var3, false, Data::SCOPE_ROOT);
		$this->assertEquals($var3, $data1->getTemplateVars('var3'));
		$this->assertEquals($var3, $data1->getTemplateVars('var3', false));
		$this->assertEquals($var1, $data2->getTemplateVars('var1'));
		$this->assertEquals(null, $data2->getTemplateVars('var1', false));
		$this->assertEquals($var2, $data2->getTemplateVars('var2'));
		$this->assertEquals(null, $data2->getTemplateVars('var2', false));
		$this->assertEquals($var3, $data2->getTemplateVars('var3'));
		$this->assertEquals(null, $data2->getTemplateVars('var3', false));

		$var1['a2'] = 'A2';
		$var2['b2'] = 'B2';
		$var3['c2'] = 'C2';
		$this->assertEquals($var3, $data1->getTemplateVars('var3'));
		$this->assertEquals($var3, $data1->getTemplateVars('var3', false));
		$this->assertEquals($var1, $data2->getTemplateVars('var1'));
		$this->assertEquals(null, $data2->getTemplateVars('var1', false));
		$this->assertEquals($var2, $data2->getTemplateVars('var2'));
		$this->assertEquals(null, $data2->getTemplateVars('var2', false));
		$this->assertEquals($var3, $data2->getTemplateVars('var3'));
		$this->assertEquals(null, $data2->getTemplateVars('var3', false));
	}

	/**
	 * Test assignByRef for TPL global scope
	 */
	public function testAssignByRefForGlobalScope()
	{
		$data1 = $this->smarty->createData($this->smarty);
		$data2 = $this->smarty->createData($data1);
		$data3 = $this->smarty->createData($data2);
		$var1 = array(
			'a1' => 'A1'
		);
		$var2 = array(
			'b1' => 'B1'
		);
		$var3 = array(
			'c1' => 'C1'
		);
		$data1->assignByRef('var1', $var1);
		$data2->assignByRef('var2', $var2, false, Data::SCOPE_PARENT);
		$data3->assignByRef('var3', $var3, false, Data::SCOPE_GLOBAL);
		$this->assertEquals($var3, $this->smarty->getTemplateVars('var3'));
		$this->assertEquals($var3, $this->smarty->getTemplateVars('var3', false));
		$this->assertEquals($var3, $data1->getTemplateVars('var3'));
		$this->assertEquals(null, $data1->getTemplateVars('var3', false));
		$this->assertEquals($var1, $data2->getTemplateVars('var1'));
		$this->assertEquals(null, $data2->getTemplateVars('var1', false));
		$this->assertEquals($var2, $data2->getTemplateVars('var2'));
		$this->assertEquals(null, $data2->getTemplateVars('var2', false));
		$this->assertEquals($var3, $data2->getTemplateVars('var3'));
		$this->assertEquals(null, $data2->getTemplateVars('var3', false));

		$var1['a2'] = 'A2';
		$var2['b2'] = 'B2';
		$var3['c2'] = 'C2';
		$this->assertEquals($var3, $this->smarty->getTemplateVars('var3'));
		$this->assertEquals($var3, $this->smarty->getTemplateVars('var3', false));
		$this->assertEquals($var3, $data1->getTemplateVars('var3'));
		$this->assertEquals(null, $data1->getTemplateVars('var3', false));
		$this->assertEquals($var1, $data2->getTemplateVars('var1'));
		$this->assertEquals(null, $data2->getTemplateVars('var1', false));
		$this->assertEquals($var2, $data2->getTemplateVars('var2'));
		$this->assertEquals(null, $data2->getTemplateVars('var2', false));
		$this->assertEquals($var3, $data2->getTemplateVars('var3'));
		$this->assertEquals(null, $data2->getTemplateVars('var3', false));
	}

	/**
	 * Test assignByRef for TPL root scope
	 */
	public function testAssignByRefForTplRootScope()
	{
		$data1 = $this->smarty->createData($this->smarty);
		$tpl1 = $this->smarty->createTemplate('eval:{$var2|json_encode}', $data1);
		$tpl2 = $this->smarty->createTemplate('eval:{$var2|json_encode}', $tpl1);
		$data2 = $tpl2->createData($tpl2);
		$var1 = array(
			'a1' => 'A1'
		);
		$var2 = array(
			'b1' => 'B1'
		);
		$var3 = array(
			'c1' => 'C1'
		);
		$data1->assignByRef('var1', $var1);
		$data2->assignByRef('var2', $var2, false, Data::SCOPE_TPL_ROOT);
		$this->assertEquals('{"b1":"B1"}', $tpl1->fetch());
		$this->assertEquals($var2, $tpl1->getTemplateVars('var2'));
		$this->assertEquals($var2, $tpl1->getTemplateVars('var2', false));
		$this->assertEquals(null, $data1->getTemplateVars('var2'));
		$this->assertEquals($var1, $data2->getTemplateVars('var1'));
		$this->assertEquals(null, $data2->getTemplateVars('var1', false));
		$this->assertEquals($var2, $data2->getTemplateVars('var2'));
		$this->assertEquals(null, $data2->getTemplateVars('var2', false));

		$var1['a2'] = 'A2';
		$var2['b2'] = 'B2';
		$this->assertEquals($var2, $tpl1->getTemplateVars('var2'));
		$this->assertEquals($var2, $tpl1->getTemplateVars('var2', false));
		$this->assertEquals(null, $data1->getTemplateVars('var2'));
		$this->assertEquals($var1, $data2->getTemplateVars('var1'));
		$this->assertEquals(null, $data2->getTemplateVars('var1', false));
		$this->assertEquals($var2, $data2->getTemplateVars('var2'));
		$this->assertEquals(null, $data2->getTemplateVars('var2', false));
	}
}
