<?php

namespace Smarty\Compile;

use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile While Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class WhileTag extends Base {

	/**
	 * Compiles code for the {while} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 * @throws \SmartyCompilerException
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		$compiler->loopNesting++;
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		$this->openTag($compiler, 'while', $compiler->nocache);
		if (!array_key_exists('if condition', $parameter)) {
			$compiler->trigger_template_error('missing while condition', null, true);
		}
		// maybe nocache because of nocache variables
		$compiler->nocache = $compiler->nocache | $compiler->tag_nocache;
		if (is_array($parameter['if condition'])) {
			if ($compiler->nocache) {
				// create nocache var to make it know for further compiling
				if (is_array($parameter['if condition']['var'])) {
					$var = $parameter['if condition']['var']['var'];
				} else {
					$var = $parameter['if condition']['var'];
				}
				$compiler->setNocacheInVariable($var);
			}
			$prefixVar = $compiler->getNewPrefixVariable();
			$assignCompiler = new Assign();
			$assignAttr = [];
			$assignAttr[]['value'] = $prefixVar;
			if (is_array($parameter['if condition']['var'])) {
				$assignAttr[]['var'] = $parameter['if condition']['var']['var'];
				$_output = "<?php while ({$prefixVar} = {$parameter[ 'if condition' ][ 'value' ]}) {?>";
				$_output .= $assignCompiler->compile(
					$assignAttr,
					$compiler,
					['smarty_internal_index' => $parameter['if condition']['var']['smarty_internal_index']]
				);
			} else {
				$assignAttr[]['var'] = $parameter['if condition']['var'];
				$_output = "<?php while ({$prefixVar} = {$parameter[ 'if condition' ][ 'value' ]}) {?>";
				$_output .= $assignCompiler->compile($assignAttr, $compiler, []);
			}
			return $_output;
		} else {
			return "<?php\n while ({$parameter['if condition']}) {?>";
		}
	}
}