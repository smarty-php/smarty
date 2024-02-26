<?php
/**
 * Smarty PHPunit tests of modifier
 */

namespace UnitTests\TemplateSource\TagTests\PluginModifier;
use PHPUnit_Smarty;

class PluginModifierSubstrTest extends PHPUnit_Smarty
{
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}

	public function testDefault()
	{
		$tpl = $this->smarty->createTemplate('string:{$v|substr:1}');
		$tpl->assign("v", "abc");
		$this->assertEquals("bc", $this->smarty->fetch($tpl));
	}

	public function testTwoArguments()
	{
		$tpl = $this->smarty->createTemplate('string:{$v|substr:1:1}');
		$tpl->assign("v", "abc");
		$this->assertEquals("b", $this->smarty->fetch($tpl));
	}

	public function testNegativeOffset()
	{
		$tpl = $this->smarty->createTemplate('string:{$v|substr:-1}');
		$tpl->assign("v", "abc");
		$this->assertEquals("c", $this->smarty->fetch($tpl));
	}

	public function testInConditional()
	{
		$tpl = $this->smarty->createTemplate('string:{if substr($v, -1) == "c"}good{else}bad{/if}');
		$tpl->assign("v", "abc");
		$this->assertEquals("good", $this->smarty->fetch($tpl));
	}

}
