<?php

namespace Smarty\Compile;

use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Foreachelse Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ForeachElse extends Base {

	/**
	 * Compiles code for the {foreachelse} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$this->getAttributes($compiler, $args);
		[$openTag, $nocache, $local, $itemVar, $restore] = $this->closeTag($compiler, ['foreach']);
		$this->openTag($compiler, 'foreachelse', ['foreachelse', $nocache, $local, $itemVar, 0]);
		$output = "<?php\n";
		if ($restore === 2) {
			$output .= "{$itemVar} = {$local}saved;\n";
		}
		$output .= "}\nif ({$itemVar}->do_else) {\n?>";
		return $output;
	}
}