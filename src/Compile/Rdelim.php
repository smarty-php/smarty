<?php
/**
 * Smarty Internal Plugin Compile Rdelim
 * Compiles the {rdelim} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile Rdelim Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Rdelim extends Ldelim {

	/**
	 * Compiles code for the {rdelim} tag
	 * This tag does output the right delimiter.
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
	 *
	 * @return string compiled code
	 * @throws \SmartyCompilerException
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		parent::compile($args, $compiler);
		return $compiler->smarty->right_delimiter;
	}
}
