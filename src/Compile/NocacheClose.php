<?php
/**
 * Smarty Internal Plugin Compile Nocache
 * Compiles the {nocache} {/nocache} tags.
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile Nocacheclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class NocacheClose extends Base {

	/**
	 * Compiles code for the {/nocache} tag
	 * This tag does not generate compiled output. It only sets a compiler flag.
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
	 *
	 * @return bool
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		$_attr = $this->getAttributes($compiler, $args);
		// leave nocache mode
		[$compiler->nocache] = $this->closeTag($compiler, ['nocache']);
		// this tag does not return compiled code
		$compiler->has_code = false;
		return true;
	}
}
