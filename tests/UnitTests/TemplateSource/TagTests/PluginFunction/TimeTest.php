<?php

namespace UnitTests\TemplateSource\TagTests\PluginFunction;
class TimeTest extends \PHPUnit_Smarty {

	public function setUp(): void {
		$this->setUpSmarty(__DIR__);
	}

	public function testBasicSyntax() {
		$this->assertStringMatchesFormat('%d', $this->smarty->fetch("string:{time()}"));
	}

	public function testInvalidParameters() {
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('Invalid number of arguments');
		$this->assertEquals("", $this->smarty->fetch("string:{time(3, 'foo')}"));
	}

}