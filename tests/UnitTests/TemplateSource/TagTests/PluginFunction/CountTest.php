<?php

namespace UnitTests\TemplateSource\TagTests\PluginFunction;
class CountTest extends \PHPUnit_Smarty {

	public function setUp(): void {
		$this->setUpSmarty(__DIR__);
	}

	public function testBasicSyntax() {
		$this->assertEquals('3', $this->smarty->fetch("string:{count([1,2,3])}"));
	}

	public function testNonRecursive() {
		$this->assertEquals('3', $this->smarty->fetch("string:{count([1,2,[3,4]])}"));
	}

	public function testRecursive() {
		$this->assertEquals('5', $this->smarty->fetch("string:{count([1,2,[3,4]], 1)}"));
	}

	public function testInvalidParameters() {
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('Invalid number of arguments');
		$this->assertEquals("", $this->smarty->fetch("string:{count()}"));
	}

}