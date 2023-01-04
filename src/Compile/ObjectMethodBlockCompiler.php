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

/**
 * Smarty Internal Plugin Compile Object Block Function Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ObjectMethodBlockCompiler extends BlockCompiler {

	/**
	 * @inheritDoc
	 */
	protected function getIsCallableCode($tag, $function): string {
		$callbackObject = "\$_smarty_tpl->smarty->registered_objects['{$tag}'][0]";
		return "(isset({$callbackObject}) && is_callable(array({$callbackObject}, '{$function}')))";
	}

	/**
	 * @inheritDoc
	 */
	protected function getFullCallbackCode($tag, $function): string {
		$callbackObject = "\$_smarty_tpl->smarty->registered_objects['{$tag}'][0]";
		return "{$callbackObject}->{$function}";
	}

}
