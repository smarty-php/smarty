<?php
/**
 * Smarty Internal Plugin Compile Function
 * Compiles the {function} {/function} tags
 *


 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

/**
 * Smarty Internal Plugin Compile Functionclose Class
 *


 */
class FunctionClose extends Base {

	/**
	 * Compiler object
	 *
	 * @var object
	 */
	private $compiler = null;

	/**
	 * Compiles code for the {/function} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param object|\Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return bool true
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		$this->compiler = $compiler;
		$saved_data = $this->closeTag($compiler, ['function']);
		$_attr = $saved_data[0];
		$_name = trim($_attr['name'], '\'"');
		$compiler->parent_compiler->tpl_function[$_name]['compiled_filepath'] =
			$compiler->parent_compiler->template->compiled->filepath;
		$compiler->parent_compiler->tpl_function[$_name]['uid'] = $compiler->template->source->uid;
		$_parameter = $_attr;
		unset($_parameter['name']);
		// default parameter
		$_paramsArray = $this->formatParamsArray($_attr);
		if (!empty($_paramsArray)) {
			$_params = 'array(' . implode(',', $_paramsArray) . ')';
			$_paramsCode = "\$params = array_merge($_params, \$params);\n";
		} else {
			$_paramsCode = '';
		}
		$_functionCode = $compiler->parser->current_buffer;
		// setup buffer for template function code
		$compiler->parser->current_buffer = new \Smarty\ParseTree\Template();
		$_funcName = "smarty_template_function_{$_name}_{$compiler->template->compiled->nocache_hash}";
		$_funcNameCaching = $_funcName . 'Smarty\Compile\Tag\Nocache';
		if ($compiler->template->compiled->has_nocache_code) {
			$compiler->parent_compiler->tpl_function[$_name]['call_name_caching'] = $_funcNameCaching;
			$output = "<?php\n";
			$output .= $compiler->cStyleComment(" {$_funcNameCaching} ") . "\n";
			$output .= "if (!function_exists('{$_funcNameCaching}')) {\n";
			$output .= "function {$_funcNameCaching} (\\Smarty\\Template \$_smarty_tpl,\$params) {\n";
			$output .= "ob_start();\n";
			$output .= "\$_smarty_tpl->compiled->has_nocache_code = true;\n";
			$output .= $_paramsCode;
			$output .= "foreach (\$params as \$key => \$value) {\n\$_smarty_tpl->assign(\$key, \$value);\n}\n";
			$output .= "\$params = var_export(\$params, true);\n";
			$output .= "echo \"/*%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%*/<?php ";
			$output .= "\\\$_smarty_tpl->smarty->getRuntime('TplFunction')->saveTemplateVariables(\\\$_smarty_tpl, '{$_name}');\nforeach (\$params as \\\$key => \\\$value) {\n\\\$_smarty_tpl->assign(\\\$key, \\\$value);\n}\n?>";
			$output .= "/*/%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%*/\";?>";
			$compiler->parser->current_buffer->append_subtree(
				$compiler->parser,
				new \Smarty\ParseTree\Tag(
					$compiler->parser,
					$output
				)
			);
			$compiler->parser->current_buffer->append_subtree($compiler->parser, $_functionCode);
			$output = "<?php echo \"/*%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%*/<?php ";
			$output .= "\\\$_smarty_tpl->smarty->getRuntime('TplFunction')->restoreTemplateVariables(\\\$_smarty_tpl, '{$_name}');?>\n";
			$output .= "/*/%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%*/\";\n?>";
			$output .= "<?php echo str_replace('{$compiler->template->compiled->nocache_hash}', \$_smarty_tpl->compiled->nocache_hash ?? '', ob_get_clean());\n";
			$output .= "}\n}\n";
			$output .= $compiler->cStyleComment("/ {$_funcName}_nocache ") . "\n\n";
			$output .= "?>\n";
			$compiler->parser->current_buffer->append_subtree(
				$compiler->parser,
				new \Smarty\ParseTree\Tag(
					$compiler->parser,
					$output
				)
			);
			$_functionCode = new \Smarty\ParseTree\Tag(
				$compiler->parser,
				preg_replace_callback(
					"/((<\?php )?echo '\/\*%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%\*\/([\S\s]*?)\/\*\/%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%\*\/';(\?>\n)?)/",
					[$this, 'removeNocache'],
					$_functionCode->to_smarty_php($compiler->parser)
				)
			);
		}
		$compiler->parent_compiler->tpl_function[$_name]['call_name'] = $_funcName;
		$output = "<?php\n";
		$output .= $compiler->cStyleComment(" {$_funcName} ") . "\n";
		$output .= "if (!function_exists('{$_funcName}')) {\n";
		$output .= "function {$_funcName}(\\Smarty\\Template \$_smarty_tpl,\$params) {\n";
		$output .= $_paramsCode;
		$output .= "foreach (\$params as \$key => \$value) {\n\$_smarty_tpl->assign(\$key, \$value);\n}\n";
		$output .= "?>\n";
		$compiler->parser->current_buffer->append_subtree(
			$compiler->parser,
			new \Smarty\ParseTree\Tag(
				$compiler->parser,
				$output
			)
		);
		$compiler->parser->current_buffer->append_subtree($compiler->parser, $_functionCode);
		$output = "<?php\n}}\n";
		$output .= $compiler->cStyleComment("/ {$_funcName} ") . "\n\n";
		$output .= "?>\n";
		$compiler->parser->current_buffer->append_subtree(
			$compiler->parser,
			new \Smarty\ParseTree\Tag(
				$compiler->parser,
				$output
			)
		);
		$compiler->parent_compiler->blockOrFunctionCode .= $compiler->parser->current_buffer->to_smarty_php($compiler->parser);
		// restore old buffer
		$compiler->parser->current_buffer = $saved_data[1];
		// restore old status
		$compiler->template->compiled->has_nocache_code = $saved_data[2];
		$compiler->template->caching = $saved_data[3];
		return true;
	}

	/**
	 * Remove nocache code
	 *
	 * @param $match
	 *
	 * @return string
	 */
	public function removeNocache($match) {
		$code =
			preg_replace(
				"/((<\?php )?echo '\/\*%%SmartyNocache:{$this->compiler->template->compiled->nocache_hash}%%\*\/)|(\/\*\/%%SmartyNocache:{$this->compiler->template->compiled->nocache_hash}%%\*\/';(\?>\n)?)/",
				'',
				$match[0]
			);
		$code = str_replace(['\\\'', '\\\\\''], ['\'', '\\\''], $code);
		return $code;
	}
}
