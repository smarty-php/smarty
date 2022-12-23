<?php
namespace Smarty\Compile\Modifier;
/**
 * Smarty str_repeat modifier plugin
 * Type:     modifier
 * Name:     str_repeat
 * Purpose:  returns string repeated times times
 *
 * @link   https://www.smarty.net/docs/en/language.modifier.str_repeat.tpl str_repeat (Smarty online manual)
 */

class StrRepeatModifierCompiler extends Base {

	public function compile($params, \Smarty\Compiler\Template $compiler) {
		return 'str_repeat((string) ' . $params[0] . ', (int) ' . $params[1] . ')';
	}

}