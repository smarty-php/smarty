<?php

namespace Smarty\Compile\Modifier;
/**
 * Smarty nl2br modifier plugin
 * Type:     modifier
 * Name:     nl2br
 * Purpose:  insert HTML line breaks before all newlines in a string
 *
 * @link   https://www.smarty.net/docs/en/language.modifier.nl2br.tpl nl2br (Smarty online manual)
 */

class Nl2brModifierCompiler extends Base {

	public function compile($params, \Smarty\Compiler\Template $compiler) {
		return 'nl2br((string) ' . $params[0] . ', (bool) ' . ($params[1] ?? true) . ')';
	}
}
