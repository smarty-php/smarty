<?php

namespace Smarty\Compile;

use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Else Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ElseTag extends Base {

	/**
	 * Compiles code for the {else} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		[$nesting, $compiler->tag_nocache] = $this->closeTag($compiler, ['if', 'elseif']);
		$this->openTag($compiler, 'else', [$nesting, $compiler->tag_nocache]);
		return '<?php } else { ?>';
	}
}