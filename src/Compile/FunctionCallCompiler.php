<?php
/**
 * Smarty Internal Plugin Compile Registered Function
 * Compiles code for the execution of a registered function
 *
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Registered Function Class
 */
class FunctionCallCompiler extends Base {
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
	public function compile($args, Template $compiler, $parameter = [], $tag = null, $function = null): string
	{
		// Compile arguments to pass on the the functionhandler
		$_params = $this->compileArguments($args);

		if ($functionHandler = $compiler->getSmarty()->getFunctionHandler($function)) {

			// not cacheable?
			$compiler->tag_nocache = $compiler->tag_nocache || !$functionHandler->isCacheable();
			$output = "\$_smarty_tpl->getSmarty()->getFunctionHandler(" . var_export($function, true) . ")";
			$output .= "->handle($_params, \$_smarty_tpl)";
		} else {
			$compiler->trigger_template_error("unknown function '{$function}'", null, true);
		}

		if (!empty($parameter['modifierlist'])) {
			$output = $compiler->compileModifier($parameter['modifierlist'], $output);
		}

		return $output;
	}

	/**
	 * Recursively compile function arguments.
	 * @param array $args array with attributes from parser
	 * @return string compiled arguments
	 */
	private function compileArguments(array $arguments): string
	{
		$params = '';

		foreach ($arguments as $key => $value) {
			$params .= var_export($key, true) . "=>";
			if (is_array($value)) {
				$params .= $this->compileArguments($value);
			} else {
				$params .= $value;
			}

			$params .= ',';
		}

		return '[' . rtrim($params, ',') . ']';
	}
}
