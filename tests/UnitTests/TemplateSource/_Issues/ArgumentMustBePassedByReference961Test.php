<?php

class ArgumentMustBePassedByReference961Test extends PHPUnit_Smarty
{

	/**
	 * @group issue961
	 */
	public function testReset()
	{
		$smarty = new Smarty();
		$smarty->registerPlugin('modifier', 'reset', 'reset');
		$templateStr = "string:{reset(\$ar)}";
		$smarty->assign('ar', [1,2,3]);
		$this->assertEquals(
			'1',
			$smarty->fetch($templateStr)
		);
	}

	/**
	 * @group issue961
	 * @deprecated
	 */
	public function testResetAsModifier()
	{
		$smarty = new Smarty();
		try {
			$templateStr = "string:{\$ar|reset}";
			$smarty->assign('ar', [1,2,3]);
			$this->assertEquals(
				'1',
				$smarty->fetch($templateStr)
			);
		} catch (Exception $e) {
		}
	}

	/**
	 * @group issue961
	 */
	public function testResetInExpression()
	{
		$smarty = new Smarty();
		$smarty->registerPlugin('modifier', 'reset', 'reset');
		$templateStr = "string:{if reset(\$ar)}ok{/if}";
		$smarty->assign('ar', [1,2,3]);
		$this->assertEquals(
			'ok',
			$smarty->fetch($templateStr)
		);
	}

	/**
	 * @group issue961
	 */
	public function testMatch()
	{
		$smarty = new Smarty();
		$smarty->registerPlugin('modifier', 'preg_match', 'preg_match');
		$templateStr = 'string:{assign var="match" value=null}{if preg_match(\'/([a-z]{4})/\', "a test", $match)}{$match.1}{/if}';
		$this->assertEquals(
			'test',
			$smarty->fetch($templateStr)
		);
	}
}