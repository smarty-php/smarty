<?php
/**
 * Smarty PHPunit tests of modifier
 */

namespace UnitTests\TemplateSource\TagTests\PluginModifier;
use PHPUnit_Smarty;

class PluginModifierImplodeTest extends PHPUnit_Smarty
{
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}
	/**
	 * @deprecated
	 */
	public function testDefault()
	{
		$tpl = $this->smarty->createTemplate('string:{$v|implode}');
		$tpl->assign("v", ["1", "2"]);
		$this->assertEquals("12", $this->smarty->fetch($tpl));
	}
	/**
	 * @deprecated
	 */
	public function testWithSeparator()
	{
		$tpl = $this->smarty->createTemplate('string:{$v|implode:","}');
		$tpl->assign("v", ["a", "b"]);
		$this->assertEquals("a,b", $this->smarty->fetch($tpl));
	}
	/**
	 * @deprecated
	 */
	public function testLegacyArgumentOrder()
	{
		$tpl = $this->smarty->createTemplate('string:{","|implode:$v}');
		$tpl->assign("v", ["a", "b"]);
		$this->assertEquals("a,b", $this->smarty->fetch($tpl));
	}
	/**
	 * @deprecated
	 */
	public function testInConditional()
	{
		$tpl = $this->smarty->createTemplate('string:{if implode($v) == "abc"}good{else}bad{/if}');
		$tpl->assign("v", ['a','b','c']);
		$this->assertEquals("good", $this->smarty->fetch($tpl));
	}
	/**
	 * @deprecated
	 */
	public function testInConditionalWithSeparator()
	{
		$tpl = $this->smarty->createTemplate('string:{if implode($v, "-") == "a-b-c"}good{else}bad{/if}');
		$tpl->assign("v", ['a','b','c']);
		$this->assertEquals("good", $this->smarty->fetch($tpl));
	}

}
