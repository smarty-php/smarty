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
