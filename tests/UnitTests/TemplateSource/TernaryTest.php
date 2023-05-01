<?php

class TernaryTest extends PHPUnit_Smarty {

	public function setUp(): void
	{
		$this->setUpSmarty('/tmp');
		$this->cleanDirs();
	}

	public function testTernaryTrue() {
		$tpl = $this->smarty->createTemplate('string:{$a ? $b : $c}');
		$tpl->assign('a', true);
		$tpl->assign('b', 'B');
		$tpl->assign('c', 'C');
		$this->assertEquals('B', $this->smarty->fetch($tpl));
	}

	public function testTernaryFalse() {
		$tpl = $this->smarty->createTemplate('string:{$a ? $b : $c}');
		$tpl->assign('a', false);
		$tpl->assign('b', 'B');
		$tpl->assign('c', 'C');
		$this->assertEquals('C', $this->smarty->fetch($tpl));
	}

	public function testShorthandTernaryTrue() {
		$tpl = $this->smarty->createTemplate('string:{$a ?: $c}');
		$tpl->assign('a', true);
		$tpl->assign('c', 'C');
		$this->assertEquals(true, $this->smarty->fetch($tpl));
	}

	public function testShorthandTernaryFalse() {
		$tpl = $this->smarty->createTemplate('string:{$a ? $b : $c}');
		$tpl->assign('a', false);
		$tpl->assign('c', 'C');
		$this->assertEquals('C', $this->smarty->fetch($tpl));
	}

}
