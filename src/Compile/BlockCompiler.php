<?php
/**
 * Smarty Internal Plugin Compile Block Plugin
 * Compiles code for the execution of block plugin
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compile\Tag\Base;

/**
 * Smarty Internal Plugin Compile Block Plugin Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class BlockCompiler extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BasePlugin
	 */
	protected $optional_attributes = ['_any'];

	/**
	 * nesting level
	 *
	 * @var int
	 */
	public $nesting = 0;

	/**
	 * Compiles code for the execution of block plugin
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 * @param string $tag name of block plugin
	 * @param string $function PHP function name
	 *
	 * @return string compiled code
	 * @throws \Smarty\CompilerException
	 * @throws \Smarty\Exception
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		if (!isset($tag[5]) || substr($tag, -5) !== 'close') {
			// opening tag of block plugin
			// check and get attributes
			$_attr = $this->getAttributes($compiler, $args);
			$this->nesting++;
			unset($_attr['nocache']);
			[$callback, $_paramsArray, $callable] = $this->setup($compiler, $_attr, $tag, $function);
			$_params = 'array(' . implode(',', $_paramsArray) . ')';
			// compile code
			$output = "<?php ";
			if (is_array($callback)) {
				$output .= "\$_block_plugin{$this->nesting} = isset({$callback[0]}) ? {$callback[0]} : null;\n";
				$callback = "\$_block_plugin{$this->nesting}{$callback[1]}";
			}
			if (isset($callable)) {
				$output .= "if (!is_callable({$callable})) {\nthrow new \\Smarty\\Exception('block tag \'{$tag}\' not callable or registered');\n}\n";
			}
			$output .= "\$_block_repeat=true;\necho {$callback}({$_params}, null, \$_smarty_tpl, \$_block_repeat);\nwhile (\$_block_repeat) {\nob_start();?>";
			$this->openTag($compiler, $tag, [$_params, $compiler->nocache, $callback]);
			// maybe nocache because of nocache variables or nocache plugin
			$compiler->nocache = $compiler->nocache | $compiler->tag_nocache;
		} else {
			// must endblock be nocache?
			if ($compiler->nocache) {
				$compiler->tag_nocache = true;
			}
			// closing tag of block plugin, restore nocache
			[$_params, $compiler->nocache, $callback] = $this->closeTag($compiler, substr($tag, 0, -5));
			// compile code
			if (!isset($parameter['modifier_list'])) {
				$mod_pre = $mod_post = $mod_content = '';
				$mod_content2 = 'ob_get_clean()';
			} else {
				$mod_content2 = "\$_block_content{$this->nesting}";
				$mod_content = "\$_block_content{$this->nesting} = ob_get_clean();\n";
				$mod_pre = "ob_start();\n";
				$mod_post = 'echo ' . $compiler->compileModifier($parameter['modifier_list'], 'ob_get_clean()')
					. ";\n";
			}
			$output =
				"<?php {$mod_content}\$_block_repeat=false;\n{$mod_pre}echo {$callback}({$_params}, {$mod_content2}, \$_smarty_tpl, \$_block_repeat);\n{$mod_post}}\n";
			$output .= '?>';
		}
		return $output;
	}

	/**
	 * Setup callback and parameter array
	 *
	 * @param \Smarty\Compiler\Template $compiler
	 * @param array $_attr attributes
	 * @param string $tag
	 * @param string $function
	 *
	 * @return array
	 */
	protected function setup(\Smarty\Compiler\Template $compiler, $_attr, $tag, $function) {
		$_paramsArray = [];
		foreach ($_attr as $_key => $_value) {
			if (is_int($_key)) {
				$_paramsArray[] = "$_key=>$_value";
			} else {
				$_paramsArray[] = "'$_key'=>$_value";
			}
		}
		return [$function, $_paramsArray, null];
	}
}
