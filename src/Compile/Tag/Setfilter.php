<?php

namespace Smarty\Compile\Tag;

/**
 * Smarty Internal Plugin Compile Setfilter Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Setfilter extends Base {

	/**
	 * Compiles code for setfilter tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		$compiler->variable_filter_stack[] = $compiler->variable_filters;
		$compiler->variable_filters = $parameter['modifier_list'];
		// this tag does not return compiled code
		$compiler->has_code = false;
		return true;
	}
}