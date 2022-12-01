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

use Smarty\Compile\PrivateBlockPlugin;
use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile Object Block Function Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class PrivateObjectBlockFunction extends PrivateBlockPlugin {

	/**
	 * Setup callback and parameter array
	 *
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler
	 * @param array $_attr attributes
	 * @param string $tag
	 * @param string $function
	 *
	 * @return array
	 */
	protected function setup(Smarty_Internal_TemplateCompilerBase $compiler, $_attr, $tag, $function) {
		$_paramsArray = [];
		foreach ($_attr as $_key => $_value) {
			if (is_int($_key)) {
				$_paramsArray[] = "$_key=>$_value";
			} else {
				$_paramsArray[] = "'$_key'=>$_value";
			}
		}
		$callback = ["\$_smarty_tpl->smarty->registered_objects['{$tag}'][0]", "->{$function}"];
		return [$callback, $_paramsArray, "array(\$_block_plugin{$this->nesting}, '{$function}')"];
	}
}
