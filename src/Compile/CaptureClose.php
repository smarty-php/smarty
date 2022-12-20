<?php
/**
 * Smarty Internal Plugin Compile Capture
 * Compiles the {capture} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Captureclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class CaptureClose extends Base {

	/**
	 * Compiles code for the {/capture} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 * @param null $parameter
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args, $parameter, '/capture');
		// must endblock be nocache?
		if ($compiler->nocache) {
			$compiler->tag_nocache = true;
		}
		[$compiler->nocache] = array_pop($compiler->_cache['capture_stack']);
		return "<?php \$_smarty_tpl->smarty->ext->_capture->close(\$_smarty_tpl);?>";
	}
}
