<?php
/**
 * Smarty PHPunit tests of modifier
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class PluginModifierImplodeTest extends PHPUnit_Smarty
{
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}

	public function testDefault()
	{
		$tpl = $this->smarty->createTemplate('string:{""|implode:$v}');
		$tpl->assign("v", ["1", "2"]);
		$this->assertEquals("12", $this->smarty->fetch($tpl));
	}
	public function testWithSeparator()
	{
		$tpl = $this->smarty->createTemplate('string:{","|implode:$v}');
		$tpl->assign("v", ["a", "b"]);
		$this->assertEquals("a,b", $this->smarty->fetch($tpl));
	}
	public function testInConditional()
	{
		$tpl = $this->smarty->createTemplate('string:{if implode("", $v) == "abc"}good{else}bad{/if}');
		$tpl->assign("v", ['a','b','c']);
		$this->assertEquals("good", $this->smarty->fetch($tpl));
	}
	public function testInConditionalWithSeparator()
	{
		$tpl = $this->smarty->createTemplate('string:{if implode("-", $v) == "a-b-c"}good{else}bad{/if}');
		$tpl->assign("v", ['a','b','c']);
		$this->assertEquals("good", $this->smarty->fetch($tpl));
	}
}
