<?php
/**
 * Smarty Internal Plugin Compile Debug
 * Compiles the {debug} tag.
 * It opens a window the the Smarty Debugging Console.
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile Debug Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Debug extends Base {

	/**
	 * Compiles code for the {debug} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param object $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes, may trigger errors
		$this->getAttributes($compiler, $args);

		// compile always as nocache
		$compiler->tag_nocache = true;
		// display debug template
		$_output =
			"<?php \$_smarty_debug = new \\Smarty\\Debug;\n \$_smarty_debug->display_debug(\$_smarty_tpl);\n";
		$_output .= "unset(\$_smarty_debug);\n?>";
		return $_output;
	}
}
