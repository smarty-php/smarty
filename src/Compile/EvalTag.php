<?php
/**
 * Smarty Internal Plugin Compile Eval
 * Compiles the {eval} tag.
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compile\Base;
use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile Eval Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class EvalTag extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	public $required_attributes = ['var'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	public $optional_attributes = ['assign'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	public $shorttag_order = ['var', 'assign'];

	/**
	 * Compiles code for the {eval} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param object $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		if (isset($_attr['assign'])) {
			// output will be stored in a smarty variable instead of being displayed
			$_assign = $_attr['assign'];
		}
		// create template object
		$_output =
			"\$_template = new {$compiler->smarty->template_class}('eval:'.{$_attr[ 'var' ]}, \$_smarty_tpl->smarty, \$_smarty_tpl);";
		//was there an assign attribute?
		if (isset($_assign)) {
			$_output .= "\$_smarty_tpl->assign($_assign,\$_template->fetch());";
		} else {
			$_output .= 'echo $_template->fetch();';
		}
		return "<?php $_output ?>";
	}
}
