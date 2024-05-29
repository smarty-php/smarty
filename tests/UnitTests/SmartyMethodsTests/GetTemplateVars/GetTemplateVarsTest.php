<?php
/**
 * Smarty PHPunit tests getTemplateVars method
 */
class GetTemplateVarsTest extends PHPUnit_Smarty
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
	 * test root getTemplateVars single value
	 */
	public function testGetSingleTemplateVarScopeRoot()
	{
		$this->smarty->assign('foo', 'bar');
		$this->smarty->assign('blar', 'buh');
		$this->assertEquals("bar", $this->smarty->getTemplateVars('foo'));
	}

	/**
	 * test root getTemplateVars all values
	 */
	public function testGetAllTemplateVarsScopeRoot()
	{
		$this->smarty->assign('foo', 'bar');
		$this->smarty->assign('blar', 'buh');
		$vars = $this->smarty->getTemplateVars();
		$this->assertTrue(is_array($vars));
		$this->assertEquals("bar", $vars['foo']);
		$this->assertEquals("buh", $vars['blar']);
	}

	/**
	 * test single variable with data object chain
	 */
	public function testGetSingleTemplateVarScopeAll()
	{
		$data1 = $this->smarty->createData($this->smarty);
		$data2 = $this->smarty->createData($data1);
		$this->smarty->assign('foo', 'bar');
		$this->smarty->assign('blar', 'buh');
		$this->assertEquals("bar", $data2->getTemplateVars('foo'));
	}

	/**
	 * test get all variables with data object chain
	 */
	public function testGetAllTemplateVarsScopeAll()
	{
		$data1 = $this->smarty->createData($this->smarty);
		$data2 = $this->smarty->createData($data1);
		$this->smarty->assign('foo', 'bar');
		$data1->assign('blar', 'buh');
		$data2->assign('foo2', 'bar2');
		$vars = $data2->getTemplateVars(null);
		$this->assertTrue(is_array($vars));
		$this->assertEquals("bar", $vars['foo']);
		$this->assertEquals("bar2", $vars['foo2']);
		$this->assertEquals("buh", $vars['blar']);
	}

	/**
	 * test get all variables with data object chain search parents disabled
	 */
	public function testGetAllTemplateVarsScopeAllNoParents()
	{
		$data1 = $this->smarty->createData($this->smarty);
		$data2 = $this->smarty->createData($data1);
		$this->smarty->assign('foo', 'bar');
		$data1->assign('blar', 'buh');
		$data2->assign('foo2', 'bar2');
		$vars = $data2->getTemplateVars(null, false);
		$this->assertTrue(is_array($vars));
		$this->assertFalse(isset($vars['foo']));
		$this->assertEquals("bar2", $vars['foo2']);
		$this->assertFalse(isset($vars['blar']));
	}

	/**
	 * test get single variables with data object chain search parents disabled
	 */
	public function testGetSingleTemplateVarsScopeAllNoParents()
	{
		error_reporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
		$data1 = $this->smarty->createData($this->smarty);
		$data2 = $this->smarty->createData($data1);
		$this->smarty->assign('foo', 'bar');
		$data1->assign('blar', 'buh');
		$data2->assign('foo2', 'bar2');
		$this->assertEquals("", $data2->getTemplateVars('foo', false));
		$this->assertEquals("bar2", $data2->getTemplateVars('foo2', false));
		$this->assertEquals("", $data2->getTemplateVars('blar', false));
	}

	/**
	 * test that variable assigned by global assign in template is included in getTemplateVars
	 */
	public function testAssignedInTemplate()
	{
		$this->smarty->fetch('string:{assign var="b" value="x" scope="global"}');
		$this->assertEquals('x', $this->smarty->getTemplateVars('b'));
	}

	/**
	 * test that getTemplateVars returns simple array of values
	 */
	public function testSimpleCallReturnsArrayWithAllValues()
	{
		$this->smarty->assign('foo', 'bar');
		$this->smarty->assign('i', 3);

		$vars = $this->smarty->getTemplateVars();

		$this->assertArrayHasKey('foo', $vars);
		$this->assertArrayHasKey('i', $vars);
		$this->assertEquals('bar', $vars['foo']);
		$this->assertEquals(3,$vars['i']);
	}

}
