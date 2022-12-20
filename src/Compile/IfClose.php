<?php
/**
 * Smarty Internal Plugin Compile If
 * Compiles the {if} {else} {elseif} {/if} tags
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compile\Assign;
use Smarty\Compile\Base;
use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Ifclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class IfClose extends Base {

	/**
	 * Compiles code for the {/if} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// must endblock be nocache?
		if ($compiler->nocache) {
			$compiler->tag_nocache = true;
		}
		[$nesting, $compiler->nocache] = $this->closeTag($compiler, ['if', 'else', 'elseif']);
		$tmp = '';
		for ($i = 0; $i < $nesting; $i++) {
			$tmp .= '}';
		}
		return "<?php {$tmp}?>";
	}
}
