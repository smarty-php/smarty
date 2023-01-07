<?php

class InArrayTest extends \PHPUnit_Smarty {

	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}

	public function testBasicSyntax()
	{
		$this->assertEquals("yay", $this->smarty->fetch("string:{if in_array(3,[3])}yay{/if}"));
	}

	public function testNotInArray()
	{
		$this->assertEquals("nay", $this->smarty->fetch("string:{if in_array(2,[3])}yay{else}nay{/if}"));
	}

	public function testInvalidParameters()
	{
		$this->expectException(\Smarty\Exception::class);
		$this->expectExceptionMessage('Invalid number of arguments');
		$this->assertEquals("", $this->smarty->fetch("string:{in_array('foo')}"));
	}

}