<?php

class TooManyShorthandAttributes949Test extends PHPUnit_Smarty
{

	public function testPregMatchAll() {
		$smarty = new \Smarty\Smarty();
		$smarty->registerPlugin('modifier', 'var_dump', 'var_dump');
		$templateStr = "eval:{\$a = 'blah'}{\$b = array()}{if var_dump('', \$a, \$b, 2)|noprint}blah{else}nah{/if}";
		$this->assertEquals(
			'nah',
			$smarty->fetch($templateStr)
		);
	}

}