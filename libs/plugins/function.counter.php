<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {counter} function plugin
 *
 * Type:     function<br>
 * Name:     counter<br>
 * Purpose:  print out a counter value
 * @link http://smarty.php.net/manual/en/language.function.counter.php {counter}
 *       (Smarty online manual)
 * @param array parameters
 * @param Smarty
 * @return string|null
 */
function smarty_function_counter($params, &$smarty)
{
    static $count = array();
    static $skipval = array();
    static $dir = array();
    static $name = "default";
    static $printval = array();

    extract($params);

    if (!isset($name)) {
		if(isset($id)) {
			$name = $id;
		} else {		
        	$name = "default";
		}
	}

    if (isset($start))
        $count[$name] = $start;
    else if (!isset($count[$name]))
        $count[$name]=1;

    if (!isset($print))
        $printval[$name]=true;
    else
        $printval[$name]=$print;
    
    if (!empty($assign)) {
        if (!isset($print)) $printval[$name] = false;
        $smarty->assign($assign, $count[$name]);
    }

    if ($printval[$name]) {
        $retval = $count[$name];
	} else {
		$retval = null;
	}

    if (isset($skip))
        $skipval[$name] = $skip;
    else if (empty($skipval[$name]))
        $skipval[$name] = 1;
    
    if (isset($direction))
        $dir[$name] = $direction;
    else if (!isset($dir[$name]))
        $dir[$name] = "up";

    if ($dir[$name] == "down")
        $count[$name] -= $skipval[$name];
    else
        $count[$name] += $skipval[$name];
	
	return $retval;
	
}

/* vim: set expandtab: */

?>
