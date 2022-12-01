<?php
/**
 * Smarty Internal Plugin Compile Function Plugin
 * Compiles code for the execution of function plugin
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compile\Base;
use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile Function Plugin Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class PrivateFunctionPlugin extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $required_attributes = [];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $optional_attributes = ['_any'];

	/**
	 * Compiles code for the execution of function plugin
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 * @param string $tag name of function plugin
	 * @param string $function PHP function name
	 *
	 * @return string compiled code
	 * @throws \SmartyCompilerException
	 * @throws \SmartyException
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		unset($_attr['nocache']);
		// convert attributes into parameter array string
		$_paramsArray = [];
		foreach ($_attr as $_key => $_value) {
			if (is_int($_key)) {
				$_paramsArray[] = "$_key=>$_value";
			} else {
				$_paramsArray[] = "'$_key'=>$_value";
			}
		}
		$_params = 'array(' . implode(',', $_paramsArray) . ')';
		// compile code
		$output = "{$function}({$_params},\$_smarty_tpl)";
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
