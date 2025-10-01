<?php

namespace Smarty\Compile;

/**
 * This class handles compiling the attributes.
 */
class AttributeCompiler
{
	/**
	 * Array of names of required attributes required by tag
	 *
	 * @var array
	 */
	protected $required_attributes = [];

	/**
	 * Array of names of optional attribute required by tag
	 * use array('_any') if there is no restriction of attributes names
	 *
	 * @var array
	 */
	protected $optional_attributes = [];

	/**
	 * Shorttag attribute order defined by its names
	 *
	 * @var array
	 */
	protected $shorttag_order = [];

	/**
	 * Array of names of valid option flags
	 *
	 * @var array
	 */
	protected $option_flags = [];

	public function __construct(
		array $required_attributes = [],
		array $optional_attributes = [],
		array $shorttag_order = [],
		array $option_flags = []
	) {
		$this->required_attributes = $required_attributes;
		$this->optional_attributes = $optional_attributes;
		$this->shorttag_order = $shorttag_order;
		$this->option_flags = $option_flags;
	}

	/**
	 * This function checks if the attributes passed are valid
	 * The attributes passed for the tag to compile are checked against the list of required and
	 * optional attributes. Required attributes must be present. Optional attributes are check against
	 * the corresponding list. The keyword '_any' specifies that any attribute will be accepted
	 * as valid
	 *
	 * @param object $compiler compiler object
	 * @param array $attributes attributes applied to the tag
	 *
	 * @return array  of mapped attributes for further processing
	 */
	public function getAttributes($compiler, $attributes)
	{
		$_indexed_attr = [];
		$options = array_fill_keys($this->option_flags, true);
		foreach ($attributes as $key => $mixed) {
			// shorthand ?
			if (!is_array($mixed)) {
				// options flag ?
				if (isset($options[trim($mixed, '\'"')])) {
					$_indexed_attr[trim($mixed, '\'"')] = true;
					// shorthand attribute ?
				} elseif (isset($this->shorttag_order[$key])) {
					$_indexed_attr[$this->shorttag_order[$key]] = $mixed;
				} else {
					// too many shorthands
					$compiler->trigger_template_error('too many shorthand attributes', null, true);
				}
				// named attribute
			} else {
				foreach ($mixed as $k => $v) {
					// options flag?
					if (isset($options[$k])) {
						if (is_bool($v)) {
							$_indexed_attr[$k] = $v;
						} else {
							if (is_string($v)) {
								$v = trim($v, '\'" ');
							}

							// Mapping array for boolean option value
							static $optionMap = [1 => true, 0 => false, 'true' => true, 'false' => false];

							if (isset($optionMap[$v])) {
								$_indexed_attr[$k] = $optionMap[$v];
							} else {
								$compiler->trigger_template_error(
									"illegal value '" . var_export($v, true) .
									"' for options flag '{$k}'",
									null,
									true
								);
							}
						}
						// must be named attribute
					} else {
						$_indexed_attr[$k] = $v;
					}
				}
			}
		}
		// check if all required attributes present
		foreach ($this->required_attributes as $attr) {
			if (!isset($_indexed_attr[$attr])) {
				$compiler->trigger_template_error("missing '{$attr}' attribute", null, true);
			}
		}
		// check for not allowed attributes
		if ($this->optional_attributes !== ['_any']) {
			$allowedAttributes = array_fill_keys(
				array_merge(
					$this->required_attributes,
					$this->optional_attributes,
					$this->option_flags
				),
				true
			);
			foreach ($_indexed_attr as $key => $dummy) {
				if (!isset($allowedAttributes[$key]) && $key !== 0) {
					$compiler->trigger_template_error("unexpected '{$key}' attribute", null, true);
				}
			}
		}
		// default 'false' for all options flags not set
		foreach ($this->option_flags as $flag) {
			if (!isset($_indexed_attr[$flag])) {
				$_indexed_attr[$flag] = false;
			}
		}

		return $_indexed_attr;
	}
}
