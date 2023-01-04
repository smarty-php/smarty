<?php
/**
 * This file is part of Smarty.
 *
 * (c) 2015 Uwe Tews
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

/**
 * Smarty Internal Plugin Compile Child Class
 *
 * @author Uwe Tews <uwe.tews@googlemail.com>
 */
class Child extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BasePlugin
	 */
	protected $optional_attributes = ['assign'];

	/**
	 * Tag name
	 *
	 * @var string
	 */
	protected $tag = 'child';

	/**
	 * Block type
	 *
	 * @var string
	 */
	protected $blockType = 'Child';

	/**
	 * Compiles code for the {child} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 * @throws \Smarty\CompilerException
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		$tag = isset($parameter[0]) ? "'{$parameter[0]}'" : "'{{$this->tag}}'";
		if (!isset($compiler->_cache['blockNesting'])) {
			$compiler->trigger_template_error(
				"{$tag} used outside {block} tags ",
				$compiler->parser->lex->taglineno
			);
		}
		$compiler->has_code = true;
		$compiler->suppressNocacheProcessing = true;
		if ($this->blockType === 'Child') {
			$compiler->_cache['blockParams'][$compiler->_cache['blockNesting']]['callsChild'] = 'true';
		}
		$_assign = $_attr['assign'] ?? null;
		$output = "<?php \n";
		if (isset($_assign)) {
			$output .= "ob_start();\n";
		}
		$output .= '$_smarty_tpl->inheritance->call' . $this->blockType . '($_smarty_tpl, $this' .
			($this->blockType === 'Child' ? '' : ", {$tag}") . ");\n";
		if (isset($_assign)) {
			$output .= "\$_smarty_tpl->assign({$_assign}, ob_get_clean());\n";
		}
		$output .= "?>\n";
		return $output;
	}
}
