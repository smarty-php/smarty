<?php
namespace Smarty\Compile\Modifier;
/**
 * Smarty string_format modifier plugin
 * Type:     modifier
 * Name:     string_format
 * Purpose:  format strings via sprintf
 *
 * @link   https://www.smarty.net/manual/en/language.modifier.string.format.php string_format (Smarty online manual)
 * @author Uwe Tews
 */

class StringFormatModifierCompiler extends Base {

	public function compile($params, \Smarty\Compiler\Template $compiler) {
		return 'sprintf(' . $params[ 1 ] . ',' . $params[ 0 ] . ')';
	}

}