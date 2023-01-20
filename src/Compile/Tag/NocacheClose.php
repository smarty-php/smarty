<?php
/**
 * Smarty Internal Plugin Compile Nocache
 * Compiles the {nocache} {/nocache} tags.
 *


 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

/**
 * Smarty Internal Plugin Compile Nocacheclose Class
 *


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
		$this->closeTag($compiler, ['nocache']);
		// this tag does not return compiled code
		$compiler->has_code = false;
		return true;
	}
}
