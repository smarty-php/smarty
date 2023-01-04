<?php

namespace Smarty\Compile;

use Smarty\Compiler\Template;

class DefaultHandlerFunctionCallCompiler extends Base {

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
	 * @param string $tag name of tag
	 * @param string $function name of function
	 *
	 * @return string compiled code
	 * @throws \Smarty\CompilerException
	 * @throws \Smarty\Exception
	 */
	public function compile($args, Template $compiler, $parameter = [], $tag = null, $function = null) {
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

		$output = "\$_smarty_tpl->smarty->runPluginFromDefaultHandler(" . var_export($function, true) .
			",'function',$_params, \$_smarty_tpl)";

		if (!empty($parameter['modifierlist'])) {
			$output = $compiler->compileModifier($parameter['modifierlist'], $output);
		}
		return "<?php echo {$output};?>\n";
	}
}