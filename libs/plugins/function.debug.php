<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {debug} function plugin
 *
 * Type:     function<br>
 * Name:     debug<br>
 * Date:     July 1, 2002<br>
 * Purpose:  popup debug window
 * @link http://smarty.php.net/manual/en/language.function.debug.php {debug}
 *       (Smarty online manual)
 * @author	 Monte Ohrt <monte@ispi.net>
 * @version  1.0
 * @param array
 * @param Smarty
 * @return string output from {@link Smarty::_generate_debug_output()}
 */
function smarty_function_debug($params, &$smarty)
{
	if($params['output']) {
		$smarty->assign('_smarty_debug_output',$params['output']);
	}
	return $smarty->_generate_debug_output();
}

/* vim: set expandtab: */

?>
