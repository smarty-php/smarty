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
function smarty_function_eval($params, &$this)
{
    extract($params);

    if (!isset($var)) {
        $this->trigger_error("eval: missing 'var' parameter");
        return;
    }
	if($var == '') {
		return;
	}

	$this->_compile_template("evaluated template", $var, $source);
	
    ob_start();
	eval('?>' . $source);
	$contents = ob_get_contents();
    ob_end_clean();

    if (!empty($assign)) {
    	$this->assign($assign, $contents);
    } else {
		return $contents;
    }
}

/* vim: set expandtab: */

?>
