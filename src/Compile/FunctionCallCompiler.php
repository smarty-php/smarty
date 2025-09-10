<?php
/**
 * Smarty Internal Plugin Compile Registered Function
 * Compiles code for the execution of a registered function
 *
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compiler\Template;
use Smarty\CompilerException;
use Smarty\FunctionHandler\AttributeFunctionHandlerInterface;

/**
 * Smarty Internal Plugin Compile Registered Function Class
 *
 */
class FunctionCallCompiler extends Base
{
	/**
	 * Array of names of required attribute required by tag
	 *
	 * @var array
	 */
	protected $required_attributes = [];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BasePlugin
	 */
	public $optional_attributes = ['_any'];

	/**
	 * Shorttag attribute order defined by its names
	 *
	 * @var array
	 */
	protected $shorttag_order = [];

	/**
	 * Array of names of valid option flags
	 *
	 * @var array
	 */
	protected $option_flags = ['nocache'];

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
		if ($functionHandler = $compiler->getSmarty()->getFunctionHandler($function)) {
			// add attributes of the function handler.
			if ($functionHandler instanceof AttributeFunctionHandlerInterface) {
				$supported_attributes = $functionHandler->getSupportedAttributes();

				foreach (['required_attributes', 'optional_attributes', 'shorttag_order', 'option_flags'] as $property) {
					$this->$property = $supported_attributes[$property];
				}
			}
			
			// check and get attributes
			$_attr = $this->getAttributes($compiler, $args);
			unset($_attr['nocache']);

			$_paramsArray = $this->formatParamsArray($_attr);
			$_params = 'array(' . implode(',', $_paramsArray) . ')';

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
}