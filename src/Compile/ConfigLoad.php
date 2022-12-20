<?php
/**
 * Smarty Internal Plugin Compile Config Load
 * Compiles the {config load} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Smarty;
use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Config Load Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ConfigLoad extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $required_attributes = ['file'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $shorttag_order = ['file', 'section'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $optional_attributes = ['section', 'scope'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $option_flags = ['nocache', 'noscope'];

	/**
	 * Valid scope names
	 *
	 * @var array
	 */
	protected $valid_scopes = [
		'local' => Smarty::SCOPE_LOCAL, 'parent' => Smarty::SCOPE_PARENT,
		'root' => Smarty::SCOPE_ROOT, 'tpl_root' => Smarty::SCOPE_TPL_ROOT,
		'smarty' => Smarty::SCOPE_SMARTY, 'global' => Smarty::SCOPE_SMARTY,
	];

	/**
	 * Compiles code for the {config_load} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 * @throws \SmartyCompilerException
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		if ($_attr['nocache'] === true) {
			$compiler->trigger_template_error('nocache option not allowed', null, true);
		}
		// save possible attributes
		$conf_file = $_attr['file'];
		$section = $_attr['section'] ?? 'null';
		// scope setup
		if ($_attr['noscope']) {
			$_scope = -1;
		} else {
			$_scope = $compiler->convertScope($_attr, $this->valid_scopes);
		}
		// create config object
		return "<?php\n\$_smarty_tpl->smarty->ext->configLoad->_loadConfigFile(\$_smarty_tpl, {$conf_file}, {$section}, {$_scope});\n?>\n";
	}
}
