<?php
/**
 * Smarty PHPunit tests of modifier
 */

namespace UnitTests\TemplateSource\TagTests\PluginModifier;
use PHPUnit_Smarty;

class PluginModifierJoinTest extends PHPUnit_Smarty
{
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}

	public function testDefault()
	{
		$tpl = $this->smarty->createTemplate('string:{$v|join}');
		$tpl->assign("v", ["1", "2"]);
		$this->assertEquals("12", $this->smarty->fetch($tpl));
	}
	public function testWithSeparator()
	{
		$tpl = $this->smarty->createTemplate('string:{$v|join:","}');
		$tpl->assign("v", ["a", "b"]);
		$this->assertEquals("a,b", $this->smarty->fetch($tpl));
	}

	public function testInConditional()
	{
		$tpl = $this->smarty->createTemplate('string:{if join($v) == "abc"}good{else}bad{/if}');
		$tpl->assign("v", ['a','b','c']);
		$this->assertEquals("good", $this->smarty->fetch($tpl));
	}

	public function testInConditionalWithSeparator()
	{
		$tpl = $this->smarty->createTemplate('string:{if join($v, "-") == "a-b-c"}good{else}bad{/if}');
		$tpl->assign("v", ['a','b','c']);
		$this->assertEquals("good", $this->smarty->fetch($tpl));
	}

}
