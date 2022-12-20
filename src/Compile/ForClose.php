<?php
/**
 * Smarty Internal Plugin Compile For
 * Compiles the {for} {forelse} {/for} tags
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Forclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ForClose extends Base {

	/**
	 * Compiles code for the {/for} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param object $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		$compiler->loopNesting--;
		// check and get attributes
		$this->getAttributes($compiler, $args);
		// must endblock be nocache?
		if ($compiler->nocache) {
			$compiler->tag_nocache = true;
		}
		[$openTag, $compiler->nocache] = $this->closeTag($compiler, ['for', 'forelse']);
		$output = "<?php }\n";
		if ($openTag !== 'forelse') {
			$output .= "}\n";
		}
		$output .= "?>";
		return $output;
	}
}
