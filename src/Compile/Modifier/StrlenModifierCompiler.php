<?php

namespace Smarty\Compile\Modifier;
/**
 * Smarty strlen modifier plugin
 * Type:     modifier
 * Name:     strlen
 * Purpose:  return the length of the given string
 *
 * @link   https://www.smarty.net/docs/en/language.modifier.strlen.tpl strlen (Smarty online manual)
 */

class StrlenModifierCompiler extends Base {

	public function compile($params, \Smarty\Compiler\Template $compiler) {
		return 'strlen((string) ' . $params[0] . ')';
	}

}