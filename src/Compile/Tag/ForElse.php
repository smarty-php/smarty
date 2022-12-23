<?php

namespace Smarty\Compile\Tag;

/**
 * Smarty Internal Plugin Compile Forelse Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ForElse extends Base {

	/**
	 * Compiles code for the {forelse} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param object $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$this->getAttributes($compiler, $args);
		[$openTag, $nocache] = $this->closeTag($compiler, ['for']);
		$this->openTag($compiler, 'forelse', ['forelse', $nocache]);
		return "<?php }} else { ?>";
	}
}