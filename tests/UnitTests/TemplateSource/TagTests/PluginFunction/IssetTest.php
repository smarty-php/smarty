<?php

namespace UnitTests\TemplateSource\TagTests\PluginFunction;
class IssetTest extends \PHPUnit_Smarty {

	public function setUp(): void {
		$this->setUpSmarty(__DIR__);
	}

	public function testBasicSyntax() {
		$this->assertEquals("nay", $this->smarty->fetch("string:{if isset(\$noSuch)}yay{else}nay{/if}"));
	}

	public function testEmptyStringIsset() {
		$this->assertEquals("yay", $this->smarty->fetch("string:{if isset('')}yay{/if}"));
	}

	public function testEmptyStringIssetModifierSyntax() {
		$this->assertEquals("yay", $this->smarty->fetch("string:{if ''|isset}yay{/if}"));
	}

	public function testFalseIsset() {
		$this->smarty->assign('test', false);
		$this->assertEquals("yay", $this->smarty->fetch("string:{if isset(\$test)}yay{/if}"));
	}

	public function testIntZeroIsset() {
		$this->smarty->assign('test', 0);
		$this->assertEquals("yay", $this->smarty->fetch("string:{if isset(\$test)}yay{/if}"));
	}

	public function testMultivar() {
		$this->smarty->assign('test', 0);
		$this->smarty->assign('test2', 'pizza');
		$this->assertEquals("yay", $this->smarty->fetch("string:{if isset(\$test, \$test2)}yay{/if}"));
	}

	public function testMultivarOneNotset() {
		$this->smarty->assign('test', 0);
		$this->assertEquals("nay", $this->smarty->fetch("string:{if isset(\$test, \$test2)}yay{else}nay{/if}"));
	}

	public function testInvalidParameters() {
		$this->expectException(\Smarty\CompilerException::class);
		$this->expectExceptionMessage('Invalid number of arguments');
		$this->assertEquals("", $this->smarty->fetch("string:{if isset()}blurp{/if}"));
	}

}