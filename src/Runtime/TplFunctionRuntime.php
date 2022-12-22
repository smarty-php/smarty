<?php

namespace Smarty\Runtime;
use Smarty_Internal_Template;
use Smarty_Internal_TemplateBase;

/**
 * TplFunction Runtime Methods callTemplateFunction
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 **/
class TplFunctionRuntime {

	/**
	 * Call template function
	 *
	 * @param \Smarty_Internal_Template $tpl template object
	 * @param string $name template function name
	 * @param array $params parameter array
	 * @param bool $nocache true if called nocache
	 *
	 * @throws \SmartyException
	 */
	public function callTemplateFunction(Smarty_Internal_Template $tpl, $name, $params, $nocache) {
		$funcParam = $tpl->tplFunctions[$name] ?? ($tpl->smarty->tplFunctions[$name] ?? null);
		if (isset($funcParam)) {
			if (!$tpl->caching || ($tpl->caching && $nocache)) {
				$function = $funcParam['call_name'];
			} else {
				if (isset($funcParam['call_name_caching'])) {
					$function = $funcParam['call_name_caching'];
				} else {
					$function = $funcParam['call_name'];
				}
			}
			if (function_exists($function)) {
				$this->saveTemplateVariables($tpl, $name);
				$function($tpl, $params);
				$this->restoreTemplateVariables($tpl, $name);
				return;
			}
			// try to load template function dynamically
			if ($this->addTplFuncToCache($tpl, $name, $function)) {
				$this->saveTemplateVariables($tpl, $name);
				$function($tpl, $params);
				$this->restoreTemplateVariables($tpl, $name);
				return;
			}
		}
		throw new SmartyException("Unable to find template function '{$name}'");
	}

	/**
	 * Register template functions defined by template
	 *
	 * @param \Smarty|\Smarty_Internal_Template|\Smarty_Internal_TemplateBase $obj
	 * @param array $tplFunctions source information array of
	 *                                                                                      template functions defined
	 *                                                                                      in template
	 * @param bool $override if true replace existing
	 *                                                                                      functions with same name
	 */
	public function registerTplFunctions(Smarty_Internal_TemplateBase $obj, $tplFunctions, $override = true) {
		$obj->tplFunctions =
			$override ? array_merge($obj->tplFunctions, $tplFunctions) : array_merge($tplFunctions, $obj->tplFunctions);
		// make sure that the template functions are known in parent templates
		if ($obj->_isSubTpl()) {
			$this->registerTplFunctions($obj->parent, $tplFunctions, false);
		} else {
			$obj->smarty->tplFunctions = $override ? array_merge($obj->smarty->tplFunctions, $tplFunctions) :
				array_merge($tplFunctions, $obj->smarty->tplFunctions);
		}
	}

	/**
	 * Return source parameter array for single or all template functions
	 *
	 * @param \Smarty_Internal_Template $tpl template object
	 * @param null|string $name template function name
	 *
	 * @return array|bool|mixed
	 */
	public function getTplFunction(Smarty_Internal_Template $tpl, $name = null) {
		if (isset($name)) {
			return $tpl->tplFunctions[$name] ?? ($tpl->smarty->tplFunctions[$name] ?? false);
		} else {
			return empty($tpl->tplFunctions) ? $tpl->smarty->tplFunctions : $tpl->tplFunctions;
		}
	}

	/**
	 * Add template function to cache file for nocache calls
	 *
	 * @param Smarty_Internal_Template $tpl
	 * @param string $_name template function name
	 * @param string $_function PHP function name
	 *
	 * @return bool
	 */
	public function addTplFuncToCache(Smarty_Internal_Template $tpl, $_name, $_function) {
		$funcParam = $tpl->tplFunctions[$_name];
		if (is_file($funcParam['compiled_filepath'])) {
			// read compiled file
			$code = file_get_contents($funcParam['compiled_filepath']);
			// grab template function
			if (preg_match("/\/\* {$_function} \*\/([\S\s]*?)\/\*\/ {$_function} \*\//", $code, $match)) {
				// grab source info from file dependency
				preg_match("/\s*'{$funcParam['uid']}'([\S\s]*?)\),/", $code, $match1);
				unset($code);
				// make PHP function known
				eval($match[0]);
				if (function_exists($_function)) {
					// search cache file template
					$tplPtr = $tpl;
					while (!isset($tplPtr->cached) && isset($tplPtr->parent)) {
						$tplPtr = $tplPtr->parent;
					}
					// add template function code to cache file
					if (isset($tplPtr->cached)) {
						$content = $tplPtr->cached->readCache($tplPtr);
						if ($content) {
							// check if we must update file dependency
							if (!preg_match("/'{$funcParam['uid']}'(.*?)'nocache_hash'/", $content, $match2)) {
								$content = preg_replace("/('file_dependency'(.*?)\()/", "\\1{$match1[0]}", $content);
							}
							$tplPtr->cached->writeCache(
								$tplPtr,
								preg_replace('/\s*\?>\s*$/', "\n", $content) .
								"\n" . preg_replace(
									[
										'/^\s*<\?php\s+/',
										'/\s*\?>\s*$/',
									],
									"\n",
									$match[0]
								)
							);
						}
					}
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Save current template variables on stack
	 *
	 * @param \Smarty_Internal_Template $tpl
	 * @param string $name stack name
	 */
	public function saveTemplateVariables(Smarty_Internal_Template $tpl, $name) {
		$tpl->_var_stack[] =
			['tpl' => $tpl->tpl_vars, 'config' => $tpl->config_vars, 'name' => "_tplFunction_{$name}"];
	}

	/**
	 * Restore saved variables into template objects
	 *
	 * @param \Smarty_Internal_Template $tpl
	 * @param string $name stack name
	 */
	public function restoreTemplateVariables(Smarty_Internal_Template $tpl, $name) {
		if (isset($tpl->_var_stack)) {
			$vars = array_pop($tpl->_var_stack);
			$tpl->tpl_vars = $vars['tpl'];
			$tpl->config_vars = $vars['config'];
		}
	}
}
