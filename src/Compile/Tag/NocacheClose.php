<?php
/**
 * Smarty Internal Plugin Compile Nocache
 * Compiles the {nocache} {/nocache} tags.
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

/**
 * Smarty Internal Plugin Compile Nocacheclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class NocacheClose extends Base {

	/**
	 * Compiles code for the {/nocache} tag
	 * This tag does not generate compiled output. It only sets a compiler flag.
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return bool
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		$_attr = $this->getAttributes($compiler, $args);
		// leave nocache mode
		[$compiler->nocache] = $this->closeTag($compiler, ['nocache']);
		// this tag does not return compiled code
		$compiler->has_code = false;
		echo "has_code set to false in " . __METHOD__ . "\n";
		return true;
	}
}
