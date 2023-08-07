<?php

class NullCoalescingTest extends PHPUnit_Smarty {

	public function setUp(): void
	{
		$this->setUpSmarty('/tmp');
		$this->cleanDirs();
	}

	public function testUndefined() {
		$tpl = $this->smarty->createTemplate('string:{$myvar ?? "undefined"}');
		$this->assertEquals('undefined', $this->smarty->fetch($tpl));
	}

	/**
	 * @dataProvider dataForOther
	 */
	public function testOther($value, $expected) {
		$tpl = $this->smarty->createTemplate('string:{$myvar ?? "undefined"}');
		$tpl->assign('myvar', $value);
		$this->assertEquals($expected, $this->smarty->fetch($tpl));
	}

	public function dataForOther() {
		return [
			[null, 'undefined'],
			['blah', 'blah'],
			['', ''],
			[false, false],
		];
	}

}
