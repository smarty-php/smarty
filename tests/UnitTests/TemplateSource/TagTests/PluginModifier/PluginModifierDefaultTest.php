<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 */

/**
 * class for modifier tests
 */
class PluginModifierDefaultTest extends PHPUnit_Smarty
{
	public function setUp()
	{
		$this->setUpSmarty(dirname(__FILE__));
	}

	/**
	 * @dataProvider        dataTestSimpleVars
	 */
	public function testSimpleVars($template, $expected)
	{
		$tpl = $this->smarty->createTemplate($template);
		$this->smarty->assign('s', 'v0');
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	/**
	 * Generates test data for ::testSimpleVars
	 */
	public function dataTestSimpleVars() {
		return array(
			array('string:{$s|default:"B"}', 'v0'),       // simple set variable
			array('string:{$u|default:"B"}', 'B'),       // simple unset variable
		);
	}

	/**
	 * @dataProvider        dataTestSimpleVarsNoDefaultValue
	 */
	public function testSimpleVarsNoDefaultValue($template, $expected)
	{
		$tpl = $this->smarty->createTemplate($template);
		$this->smarty->assign('s', 'v0');
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	/**
	 * Generates test data for ::testSimpleVarsNoDefaultValue
	 */
	public function dataTestSimpleVarsNoDefaultValue() {
		return array(
			array('string:{$s|default}', 'v0'),       // simple set variable
			array('string:{$u|default}', ''),       // simple unset variable
		);
	}

	public function testEmptyString()
	{
		$tpl = $this->smarty->createTemplate('string:{$s|default:"B"}');
		$this->smarty->assign('s', '');
		$this->assertEquals('B', $this->smarty->fetch($tpl));
	}

	public function testNull()
	{
		$tpl = $this->smarty->createTemplate('string:{$s|default:"B"}');
		$this->smarty->assign('s', null);
		$this->assertEquals('B', $this->smarty->fetch($tpl));
	}

	public function testFalse()
	{
		$tpl = $this->smarty->createTemplate('string:{$s|default:"B"}');
		$this->smarty->assign('s', false);
		$this->assertEquals('B', $this->smarty->fetch($tpl));
	}

	public function testFunctionCall()
	{
		$tpl = $this->smarty->createTemplate('string:{strlen("a")|default:"B"}');
		$this->assertEquals('1', $this->smarty->fetch($tpl));
	}

	public function testFunctionCallEmptyString()
	{
		$tpl = $this->smarty->createTemplate('string:{trim("")|default:"B"}');
		$this->assertEquals('B', $this->smarty->fetch($tpl));
	}

	/**
	 * @dataProvider        dataTestArrayObjectCombinations
	 */
	public function testArrayObjectCombinations($template, $expected)
	{
		$tpl = $this->smarty->createTemplate($template);
		$this->smarty->assign('a', array(
			'k' => (object) array ('v' => 'v3'),
			'v' => 'v1'
		));
		$this->smarty->assign('o', (object) array (
			'p' => array('v' => 'v4'),
			'v' => 'v2'
		));
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	/**
	 * Generates test data for ::testArrayObjectCombinations
	 */
	public function dataTestArrayObjectCombinations() {
		return array(
			array('string:{$a.v|default:"B"}', 'v1'),     // array key set
			array('string:{$o->v|default:"B"}', 'v2'),    // object property set
			array('string:{$a.k->v|default:"B"}', 'v3'),  // complex combi of array and key access
			array('string:{$o->p.v|default:"B"}', 'v4'),  // complex combi of array and key access

			array('string:{$a.c|default:"B"}', 'B'),     // array key not set
			array('string:{$u.c|default:"B"}', 'B'),     // array not set, referencing key
			array('string:{$o->u|default:"B"}', 'B'),    // object property not set
			array('string:{$u->u|default:"B"}', 'B'),    // object not set
			array('string:{$a.k->u|default:"B"}', 'B'),  // complex combi of array and key access with something unset
			array('string:{$a.u->u|default:"B"}', 'B'),  // complex combi of array and key access with something unset
			array('string:{$o->p.u|default:"B"}', 'B'),  // complex combi of array and key access with something unset
			array('string:{$o->u.u|default:"B"}', 'B'),  // complex combi of array and key access with something unset

		);
	}



}
