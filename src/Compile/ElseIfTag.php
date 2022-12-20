<?php

namespace Smarty\Compile;

use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile ElseIf Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ElseIfTag extends Base {

	/**
	 * Compiles code for the {elseif} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 * @throws \SmartyCompilerException
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		[$nesting, $compiler->tag_nocache] = $this->closeTag($compiler, ['if', 'elseif']);
		if (!isset($parameter['if condition'])) {
			$compiler->trigger_template_error('missing elseif condition', null, true);
		}
		$assignCode = '';
		$var = '';
		if (is_array($parameter['if condition'])) {
			$condition_by_assign = true;
			if (is_array($parameter['if condition']['var'])) {
				$var = $parameter['if condition']['var']['var'];
			} else {
				$var = $parameter['if condition']['var'];
			}
			if ($compiler->nocache) {
				// create nocache var to make it know for further compiling
				$compiler->setNocacheInVariable($var);
			}
			$prefixVar = $compiler->getNewPrefixVariable();
			$assignCode = "<?php {$prefixVar} = {$parameter[ 'if condition' ][ 'value' ]};?>\n";
			$assignCompiler = new Assign();
			$assignAttr = [];
			$assignAttr[]['value'] = $prefixVar;
			if (is_array($parameter['if condition']['var'])) {
				$assignAttr[]['var'] = $parameter['if condition']['var']['var'];
				$assignCode .= $assignCompiler->compile(
					$assignAttr,
					$compiler,
					['smarty_internal_index' => $parameter['if condition']['var']['smarty_internal_index']]
				);
			} else {
				$assignAttr[]['var'] = $parameter['if condition']['var'];
				$assignCode .= $assignCompiler->compile($assignAttr, $compiler, []);
			}
		} else {
			$condition_by_assign = false;
		}
		$prefixCode = $compiler->getPrefixCode();
		if (empty($prefixCode)) {
			if ($condition_by_assign) {
				$this->openTag($compiler, 'elseif', [$nesting + 1, $compiler->tag_nocache]);
				$_output = $compiler->appendCode("<?php } else {\n?>", $assignCode);
				return $compiler->appendCode($_output, "<?php if ({$prefixVar}) {?>");
			} else {
				$this->openTag($compiler, 'elseif', [$nesting, $compiler->tag_nocache]);
				return "<?php } elseif ({$parameter['if condition']}) {?>";
			}
		} else {
			$_output = $compiler->appendCode("<?php } else {\n?>", $prefixCode);
			$this->openTag($compiler, 'elseif', [$nesting + 1, $compiler->tag_nocache]);
			if ($condition_by_assign) {
				$_output = $compiler->appendCode($_output, $assignCode);
				return $compiler->appendCode($_output, "<?php if ({$prefixVar}) {?>");
			} else {
				return $compiler->appendCode($_output, "<?php if ({$parameter['if condition']}) {?>");
			}
		}
	}
}