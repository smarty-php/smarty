<?php

namespace UnitTests\TemplateSource\TagTests\PluginFunction;
class IsArrayTest extends \PHPUnit_Smarty {

	public function setUp(): void {
		$this->setUpSmarty(__DIR__);
	}

	public function testBasicSyntax() {
		$this->assertEquals("yay", $this->smarty->fetch("string:{if is_array([3])}yay{/if}"));
	}

	public function testIntNotIsArray() {
		$this->assertEquals("nay", $this->smarty->fetch("string:{if is_array(2)}yay{else}nay{/if}"));
	}

	public function testStringNotIsArray() {
		$this->assertEquals("nay", $this->smarty->fetch("string:{if is_array('foo')}yay{else}nay{/if}"));
	}

	public function testInvalidParameters() {
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('Invalid number of arguments');
		$this->assertEquals("", $this->smarty->fetch("string:{is_array(3, 'foo')}"));
	}

}