<?php

namespace Smarty\Compile;

use Smarty\Compiler\Template;

/**
 * Smarty Internal Plugin Compile Function Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class FunctionTag extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $required_attributes = ['name'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $shorttag_order = ['name'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Base
	 */
	protected $optional_attributes = ['_any'];

	/**
	 * Compiles code for the {function} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return bool true
	 * @throws \SmartyCompilerException
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		if ($_attr['nocache'] === true) {
			$compiler->trigger_template_error('nocache option not allowed', null, true);
		}
		unset($_attr['nocache']);
		$_name = trim($_attr['name'], '\'"');

		if (!preg_match('/^[a-zA-Z0-9_\x80-\xff]+$/', $_name)) {
			$compiler->trigger_template_error("Function name contains invalid characters: {$_name}", null, true);
		}

		$compiler->parent_compiler->tpl_function[$_name] = [];
		$save = [
			$_attr, $compiler->parser->current_buffer, $compiler->template->compiled->has_nocache_code,
			$compiler->template->caching,
		];
		$this->openTag($compiler, 'function', $save);
		// Init temporary context
		$compiler->parser->current_buffer = new \Smarty\ParseTree\Template();
		$compiler->template->compiled->has_nocache_code = false;
		return true;
	}
}