<?php
/**
 * Smarty Internal Plugin Compile Object Block Function
 * Compiles code for registered objects as block function
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Object Block Function Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ObjectMethodBlockCompiler extends BlockCompiler {

	/**
	 * Setup callback and parameter array
	 *
	 * @param Template $compiler
	 * @param array $_attr attributes
	 * @param string $tag
	 * @param string $function
	 *
	 * @return array
	 */
	protected function setup(Template $compiler, $_attr, $tag, $function) {
		$_paramsArray = $this->formatParamsArray($_attr);
		$callback = ["\$_smarty_tpl->smarty->registered_objects['{$tag}'][0]", "->{$function}"];
		return [$callback, $_paramsArray, "array(\$_block_plugin{$this->nesting}, '{$function}')"];
	}
}
