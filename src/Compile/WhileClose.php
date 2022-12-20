<?php
/**
 * Smarty Internal Plugin Compile While
 * Compiles the {while} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Whileclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class WhileClose extends Base {

	/**
	 * Compiles code for the {/while} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		$compiler->loopNesting--;
		// must endblock be nocache?
		if ($compiler->nocache) {
			$compiler->tag_nocache = true;
		}
		$compiler->nocache = $this->closeTag($compiler, ['while']);
		return "<?php }?>\n";
	}
}
