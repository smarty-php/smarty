<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {eval} function plugin
 *
 * Type:     function<br>
 * Name:     eval<br>
 * Purpose:  evaluate a template variable as a template<br>
 * @link http://smarty.php.net/manual/en/language.function.eval.php {eval}
 *       (Smarty online manual)
 * @param array
 * @param Smarty
 */
function smarty_function_eval($params, &$smarty)
{

    if (!isset($params['var'])) {
        $smarty->trigger_error("eval: missing 'var' parameter");
        return;
    }

	if($params['var'] == '') {
		return;
	}

	$smarty->_compile_source('evaluated template', $params['var'], $_var_compiled);

    ob_start();
    $this =& $smarty; /* this should be done nicer, maybe */
	eval('?>' . $_var_compiled);
	$_contents = ob_get_contents();
    ob_end_clean();

    if (!empty($assign)) {
    	$smarty->assign($assign, $_contents);
    } else {
		return $_contents;
    }
}

/* vim: set expandtab: */

?>
