<?php
/**
 * Smarty Internal Plugin Compile Modifier
 * Compiles code for modifier execution
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

/**
 * Smarty Internal Plugin Compile Modifier Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class PrivateModifier extends Base {

	/**
	 * Compiles code for modifier execution
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 * @throws \Smarty\CompilerException
	 * @throws \Smarty\Exception
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		$output = $parameter['value'];
		// loop over list of modifiers
		foreach ($parameter['modifierlist'] as $single_modifier) {
			/* @var string $modifier */
			$modifier = $single_modifier[0];
			$single_modifier[0] = $output;
			$params = implode(',', $single_modifier);
			// check if we know already the type of modifier
			if (isset($compiler->known_modifier_type[$modifier])) {
				$modifier_types = [$compiler->known_modifier_type[$modifier]];
			} else {
				$modifier_types = [1, 2, 3, 4, 6];
			}
			foreach ($modifier_types as $type) {
				switch ($type) {
					case 1:
						// registered modifier
						if (isset($compiler->smarty->registered_plugins[\Smarty\Smarty::PLUGIN_MODIFIER][$modifier])) {
							if (is_callable($compiler->smarty->registered_plugins[\Smarty\Smarty::PLUGIN_MODIFIER][$modifier][0])) {
								$output =
									sprintf(
										'call_user_func_array($_smarty_tpl->registered_plugins[ \'%s\' ][ %s ][ 0 ], array( %s ))',
										\Smarty\Smarty::PLUGIN_MODIFIER,
										var_export($modifier, true),
										$params
									);
								$compiler->known_modifier_type[$modifier] = $type;
								break 2;
							}
						}
						break;
					case 2:
						// registered modifier compiler
						if (isset($compiler->smarty->registered_plugins[\Smarty\Smarty::PLUGIN_MODIFIERCOMPILER][$modifier][0])) {
							$output =
								call_user_func(
									$compiler->smarty->registered_plugins[\Smarty\Smarty::PLUGIN_MODIFIERCOMPILER][$modifier][0],
									$single_modifier,
									$compiler->smarty
								);
							$compiler->known_modifier_type[$modifier] = $type;
							break 2;
						}
						break;
					case 3:
						// modifiercompiler plugin
						// check if modifier allowed
						if ($compiler->getModifierCompiler($modifier)) {
							$output = $compiler->compileModifier($modifier, $single_modifier);
						}
						$compiler->known_modifier_type[$modifier] = $type;
						break 2;
					case 4:
						// modifier plugin
						if ($function = $compiler->getPlugin($modifier, \Smarty\Smarty::PLUGIN_MODIFIER)) {
							// check if modifier allowed
							if (!is_object($compiler->smarty->security_policy)
								|| $compiler->smarty->security_policy->isTrustedModifier($modifier, $compiler)
							) {
								$output = "{$function}({$params})";
							}
							$compiler->known_modifier_type[$modifier] = $type;
							break 2;
						}
						break;
					// Case 5 was a direct call to a callable (usually PHP function).
					// This was removed in Smarty v5 after being deprecated in 4.3.
					case 6:
						// default plugin handler
						if (isset($compiler->default_handler_plugins[\Smarty\Smarty::PLUGIN_MODIFIER][$modifier])
							|| (is_callable($compiler->smarty->default_plugin_handler_func)
								&& $compiler->getPluginFromDefaultHandler($modifier, \Smarty\Smarty::PLUGIN_MODIFIER))
						) {
							$function = $compiler->default_handler_plugins[\Smarty\Smarty::PLUGIN_MODIFIER][$modifier][0];
							// check if modifier allowed
							if (!is_object($compiler->smarty->security_policy)
								|| $compiler->smarty->security_policy->isTrustedModifier($modifier, $compiler)
							) {
								if (!is_array($function)) {
									$output = "{$function}({$params})";
								} else {
									if (is_object($function[0])) {
										$output = $function[0] . '->' . $function[1] . '(' . $params . ')';
									} else {
										$output = $function[0] . '::' . $function[1] . '(' . $params . ')';
									}
								}
							}
							$compiler->known_modifier_type[$modifier] = $type;
							break 2;
						}
				}
			}
			if (!isset($compiler->known_modifier_type[$modifier])) {
				$compiler->trigger_template_error("unknown modifier '{$modifier}'", null, true);
			}
		}
		return $output;
	}
}
