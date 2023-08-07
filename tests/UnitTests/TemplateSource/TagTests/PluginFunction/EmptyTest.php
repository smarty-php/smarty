<?php

namespace UnitTests\TemplateSource\TagTests\PluginFunction;
class EmptyTest extends \PHPUnit_Smarty {

	public function setUp(): void {
		$this->setUpSmarty(__DIR__);
	}

	public function testBasicSyntax() {
		$this->assertEquals("yay", $this->smarty->fetch("string:{if empty(\$noSuch)}yay{/if}"));
	}

	public function testEmptyStringIsEmpty() {
		$this->assertEquals("yay", $this->smarty->fetch("string:{if empty('')}yay{/if}"));
	}

	public function testFalseIsEmpty() {
		$this->smarty->assign('test', false);
		$this->assertEquals("yay", $this->smarty->fetch("string:{if empty(\$test)}yay{/if}"));
	}

	public function testIntZeroIsEmpty() {
		$this->smarty->assign('test', 0);
		$this->assertEquals("yay", $this->smarty->fetch("string:{if empty(\$test)}yay{/if}"));
	}

	public function testStringZeroIsEmpty() {
		$this->smarty->assign('test', '0');
		$this->assertEquals("yay", $this->smarty->fetch("string:{if empty(\$test)}yay{/if}"));
	}

	public function testIntThreeIsNotEmpty() {
		$this->smarty->assign('test', 3);
		$this->assertEquals("nay", $this->smarty->fetch("string:{if empty(\$test)}yay{else}nay{/if}"));
	}

	public function testInvalidParameters() {
		$this->expectException(\Smarty\CompilerException::class);
		$this->expectExceptionMessage('Invalid number of arguments');
		$this->assertEquals("", $this->smarty->fetch("string:{empty(3, 'foo')}"));
	}

}