<?php
/**
 * Smarty Internal Plugin Compile Registered Function
 * Compiles code for the execution of a registered function
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compile\Tag\Base;
use Smarty\Compiler\Template;
use Smarty\Smarty;

/**
 * Smarty Internal Plugin Compile Registered Function Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class FunctionCallCompiler extends Base {

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


		$functionHandler = $compiler->smarty->getFunctionHandler($function);

		// not cacheable?
		$compiler->tag_nocache = $compiler->tag_nocache || !$functionHandler->isCacheable();
		// convert attributes into parameter array string
		$_paramsArray = [];
		foreach ($_attr as $_key => $_value) {
			if (is_int($_key)) {
				$_paramsArray[] = "$_key=>$_value";
			} elseif ($compiler->template->caching && in_array($_key, $functionHandler->getCacheAttributes())) {
				$_value = str_replace('\'', "^#^", $_value);
				$_paramsArray[] = "'$_key'=>^#^.var_export($_value,true).^#^";
			} else {
				$_paramsArray[] = "'$_key'=>$_value";
			}
		}
		$_params = 'array(' . implode(',', $_paramsArray) . ')';

		$output = "\$_smarty_tpl->smarty->getFunctionHandler(" . var_export($function, true) . ")";
		$output .= "->handle($_params, \$_smarty_tpl)";

		if (!empty($parameter['modifierlist'])) {
			$output = $compiler->compileModifier($parameter['modifierlist'], $output);
		}
		return "<?php echo {$output};?>\n";
	}
}
