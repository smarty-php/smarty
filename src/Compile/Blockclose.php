<?php

namespace Smarty\Compile;

use Smarty\ParseTree\Template;
use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile BlockClose Class
 */
class Blockclose extends Inheritance {

	/**
	 * Compiles code for the {/block} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return bool true
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = array(), $tag = null, $function = null)
	{
		[$_attr, $_nocache, $_buffer, $_has_nocache_code, $_caching] = $this->closeTag($compiler, ['block']);
		// init block parameter
		$_block = $compiler->_cache['blockParams'][$compiler->_cache['blockNesting']];
		unset($compiler->_cache['blockParams'][$compiler->_cache['blockNesting']]);
		$_name = $_attr['name'];
		$_assign = isset($_attr['assign']) ? $_attr['assign'] : null;
		unset($_attr['assign'], $_attr['name']);
		foreach ($_attr as $name => $stat) {
			if ((is_bool($stat) && $stat !== false) || (!is_bool($stat) && $stat !== 'false')) {
				$_block[$name] = 'true';
			}
		}
		$_className = $compiler->_cache['blockClass'][$compiler->_cache['blockNesting']];
		// get compiled block code
		$_functionCode = $compiler->parser->current_buffer;
		// setup buffer for template function code
		$compiler->parser->current_buffer = new Template();
		$output = "<?php\n";
		$output .= $compiler->cStyleComment(" {block {$_name}} ") . "\n";
		$output .= "class {$_className} extends \\Smarty\\Block\n";
		$output .= "{\n";
		foreach ($_block as $property => $value) {
			$output .= "public \${$property} = " . var_export($value, true) . ";\n";
		}
		$output .= "public function callBlock(Smarty_Internal_Template \$_smarty_tpl) {\n";
		if ($compiler->template->compiled->has_nocache_code) {
			$output .= "\$_smarty_tpl->cached->hashes['{$compiler->template->compiled->nocache_hash}'] = true;\n";
		}
		if (isset($_assign)) {
			$output .= "ob_start();\n";
		}
		$output .= "?>\n";
		$compiler->parser->current_buffer->append_subtree(
			$compiler->parser,
			new \Smarty\ParseTree\Tag(
				$compiler->parser,
				$output
			)
		);
		$compiler->parser->current_buffer->append_subtree($compiler->parser, $_functionCode);
		$output = "<?php\n";
		if (isset($_assign)) {
			$output .= "\$_smarty_tpl->assign({$_assign}, ob_get_clean());\n";
		}
		$output .= "}\n";
		$output .= "}\n";
		$output .= $compiler->cStyleComment(" {/block {$_name}} ") . "\n\n";
		$output .= "?>\n";
		$compiler->parser->current_buffer->append_subtree(
			$compiler->parser,
			new \Smarty\ParseTree\Tag(
				$compiler->parser,
				$output
			)
		);
		$compiler->blockOrFunctionCode .= $compiler->parser->current_buffer->to_smarty_php($compiler->parser);
		$compiler->parser->current_buffer = new Template();
		// restore old status
		$compiler->template->compiled->has_nocache_code = $_has_nocache_code;
		$compiler->tag_nocache = $compiler->nocache;
		$compiler->nocache = $_nocache;
		$compiler->parser->current_buffer = $_buffer;
		$output = "<?php \n";
		if ($compiler->_cache['blockNesting'] === 1) {
			$output .= "\$_smarty_tpl->inheritance->instanceBlock(\$_smarty_tpl, '$_className', $_name);\n";
		} else {
			$output .= "\$_smarty_tpl->inheritance->instanceBlock(\$_smarty_tpl, '$_className', $_name, \$this->tplIndex);\n";
		}
		$output .= "?>\n";
		--$compiler->_cache['blockNesting'];
		if ($compiler->_cache['blockNesting'] === 0) {
			unset($compiler->_cache['blockNesting']);
		}
		$compiler->has_code = true;
		$compiler->suppressNocacheProcessing = true;
		return $output;
	}
}