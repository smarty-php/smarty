<?php

class OperatorsTest extends PHPUnit_Smarty {
	public function setUp(): void
	{
		$this->setUpSmarty(__DIR__);
	}

	public function testInit()
	{
		$this->cleanDirs();
	}

	/**
	 * @group issue861
	 */
	public function testTernaries() {
		$this->assertEquals('2 equals 2', $this->smarty->fetch("string:{(2 === 2) ? '2 equals 2' : '2 ain\'t 2'}"));
		$this->assertEquals('3 equals 3', $this->smarty->fetch("string:{(3 == 3) ? '3 equals 3' : '3 ain\'t 3'}"));
		$this->assertEquals('4 equals 4', $this->smarty->fetch("string:{(4 !== 4) ? '4 ain\'t 4' : '4 equals 4'}"));
	}

}