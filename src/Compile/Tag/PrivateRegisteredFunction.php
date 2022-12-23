<?php
/**
 * Smarty Internal Plugin Compile Registered Function
 * Compiles code for the execution of a registered function
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

use Smarty\Compiler\Template;
use Smarty\Smarty;

/**
 * Smarty Internal Plugin Compile Registered Function Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class PrivateRegisteredFunction extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BasePlugin
	 */
	public $optional_attributes = ['_any'];

	/**
	 * Compiles code for the execution of a registered function
	 *
	 * @param array $args array with attributes from parser
	 * @param Template $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 * @param string $tag name of function
	 *
	 * @return string compiled code
	 * @throws \Smarty\CompilerException
	 * @throws \Smarty\Exception
	 */
	public function compile($args, Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		unset($_attr['nocache']);
		if (isset($compiler->smarty->registered_plugins[Smarty::PLUGIN_FUNCTION][$tag])) {
			$tag_info = $compiler->smarty->registered_plugins[Smarty::PLUGIN_FUNCTION][$tag];
			$is_registered = true;
		} else {
			$tag_info = $compiler->default_handler_plugins[Smarty::PLUGIN_FUNCTION][$tag];
			$is_registered = false;
		}
		// not cacheable?
		$compiler->tag_nocache = $compiler->tag_nocache || !$tag_info[1];
		// convert attributes into parameter array string
		$_paramsArray = [];
		foreach ($_attr as $_key => $_value) {
			if (is_int($_key)) {
				$_paramsArray[] = "$_key=>$_value";
			} elseif ($compiler->template->caching && in_array($_key, $tag_info[2])) {
				$_value = str_replace('\'', "^#^", $_value);
				$_paramsArray[] = "'$_key'=>^#^.var_export($_value,true).^#^";
			} else {
				$_paramsArray[] = "'$_key'=>$_value";
			}
		}
		$_params = 'array(' . implode(',', $_paramsArray) . ')';
		// compile code
		if ($is_registered) {
			$output =
				"call_user_func_array( \$_smarty_tpl->smarty->registered_plugins[\\Smarty\\Smarty::PLUGIN_FUNCTION]['{$tag}'][0], array( {$_params},\$_smarty_tpl ) )";
		} else {
			$function = $tag_info[0];
			if (!is_array($function)) {
				$output = "{$function}({$_params},\$_smarty_tpl)";
			} else {
				$output = "{$function[0]}::{$function[1]}({$_params},\$_smarty_tpl)";
			}
		}
		if (!empty($parameter['modifierlist'])) {
			$output = $compiler->compileTag(
				'private_modifier',
				[],
				[
					'modifierlist' => $parameter['modifierlist'],
					'value' => $output,
				]
			);
		}
		$output = "<?php echo {$output};?>\n";
		return $output;
	}
}
