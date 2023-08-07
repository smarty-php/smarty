<?php

namespace Smarty\Compile\Modifier;
/**
 * Smarty round modifier plugin
 * Type:     modifier
 * Name:     round
 * Purpose:  Returns the rounded value of num to specified precision (number of digits after the decimal point)
 *
 * @link   https://www.smarty.net/docs/en/language.modifier.round.tpl round (Smarty online manual)
 */

class RoundModifierCompiler extends Base {

	public function compile($params, \Smarty\Compiler\Template $compiler) {
		return 'round((float) ' . $params[0] . ', (int) ' . ($params[1] ?? 0) . ', (int) ' . ($params[2] ?? PHP_ROUND_HALF_UP) . ')';
	}

}