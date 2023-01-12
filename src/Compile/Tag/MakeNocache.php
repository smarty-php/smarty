<?php
/**
 * Smarty Internal Plugin Compile Make_Nocache
 * Compiles the {make_nocache} tag
 *


 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

/**
 * Smarty Internal Plugin Compile Make_Nocache Class
 *


 */
class MakeNocache extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BaseCompiler
	 */
	protected $option_flags = [];

	/**
	 * Array of names of required attribute required by tag
	 *
	 * @var array
	 */
	protected $required_attributes = ['var'];

	/**
	 * Shorttag attribute order defined by its names
	 *
	 * @var array
	 */
	protected $shorttag_order = ['var'];

	/**
	 * Compiles code for the {make_nocache} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		if ($compiler->template->caching) {
			$output = "<?php \$_smarty_tpl->smarty->getRuntime('MakeNocache')->save(\$_smarty_tpl, {$_attr[ 'var' ]});\n?>\n";
			$compiler->template->getCompiled()->setNocacheCode(true);
			$compiler->suppressNocacheProcessing = true;
			return $output;
		} else {
			return true;
		}
	}
}
