<?php

namespace Smarty\Runtime;
use Smarty\Exception;
use Smarty\Template;
use Smarty\TemplateBase;

/**
 * TplFunction Runtime Methods callTemplateFunction
 *


 * @author     Uwe Tews
 **/
class TplFunctionRuntime {

	/**
	 * Call template function
	 *
	 * @param \Smarty\Template $tpl template object
	 * @param string $name template function name
	 * @param array $params parameter array
	 * @param bool $nocache true if called nocache
	 *
	 * @throws \Smarty\Exception
	 */
	public function callTemplateFunction(Template $tpl, $name, $params, $nocache) {
		$funcParam = $tpl->tplFunctions[$name] ?? ($tpl->getSmarty()->tplFunctions[$name] ?? null);
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
		throw new \Smarty\Exception("Unable to find template function '{$name}'");
	}

	/**
	 * Register template functions defined by template
	 *
	 * @param \Smarty|\Smarty\Template|\Smarty\TemplateBase $obj
	 * @param array $tplFunctions source information array of
	 *                                                                                      template functions defined
	 *                                                                                      in template
	 * @param bool $override if true replace existing
	 *                                                                                      functions with same name
	 */
	public function registerTplFunctions(TemplateBase $obj, $tplFunctions, $override = true) {
		$obj->tplFunctions =
			$override ? array_merge($obj->tplFunctions, $tplFunctions) : array_merge($tplFunctions, $obj->tplFunctions);
		// make sure that the template functions are known in parent templates
		if ($obj->_isSubTpl()) {
			$this->registerTplFunctions($obj->parent, $tplFunctions, false);
		} else {
			$obj->getSmarty()->tplFunctions = $override ? array_merge($obj->getSmarty()->tplFunctions, $tplFunctions) :
				array_merge($tplFunctions, $obj->getSmarty()->tplFunctions);
		}
	}

	/**
	 * Return source parameter array for single or all template functions
	 *
	 * @param \Smarty\Template $tpl template object
	 * @param null|string $name template function name
	 *
	 * @return array|bool|mixed
	 */
	public function getTplFunction(Template $tpl, $name = null) {
		if (isset($name)) {
			return $tpl->tplFunctions[$name] ?? ($tpl->getSmarty()->tplFunctions[$name] ?? false);
		} else {
			return empty($tpl->tplFunctions) ? $tpl->getSmarty()->tplFunctions : $tpl->tplFunctions;
		}
	}

	/**
	 * Add template function to cache file for nocache calls
	 *
	 * @param Template $tpl
	 * @param string $_name template function name
	 * @param string $_function PHP function name
	 *
	 * @return bool
	 * @throws Exception
	 */
	private function addTplFuncToCache(Template $tpl, $_name, $_function) {
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

					// Some magic code existed here, testing if the cached property had been set
					// and then bubbling up until it found a parent template that had the cached property.
					// This is no longer possible, so somehow this might break.

					// add template function code to cache file
					$content = $tpl->getCached()->readCache($tpl);
					if ($content) {
						// check if we must update file dependency
						if (!preg_match("/'{$funcParam['uid']}'(.*?)'nocache_hash'/", $content, $match2)) {
							$content = preg_replace("/('file_dependency'(.*?)\()/", "\\1{$match1[0]}", $content);
						}
						$tpl->getCached()->writeCache(
							$tpl,
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
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Save current template variables on stack
	 *
	 * @param \Smarty\Template $tpl
	 * @param string $name stack name
	 */
	public function saveTemplateVariables(Template $tpl, $name) {
		$tpl->_var_stack[] =
			['tpl' => $tpl->tpl_vars, 'config' => $tpl->config_vars, 'name' => "_tplFunction_{$name}"];
	}

	/**
	 * Restore saved variables into template objects
	 *
	 * @param \Smarty\Template $tpl
	 * @param string $name stack name
	 */
	public function restoreTemplateVariables(Template $tpl, $name) {
		if (isset($tpl->_var_stack)) {
			$vars = array_pop($tpl->_var_stack);
			$tpl->tpl_vars = $vars['tpl'];
			$tpl->config_vars = $vars['config'];
		}
	}
}
