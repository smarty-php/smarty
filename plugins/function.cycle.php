<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     cycle
 * Version:  1.1
 * Author:	 Monte Ohrt <monte@ispi.net>
 * Credits:  Mark Priatel <mpriatel@rogers.com>
 *           Gerard <gerard@interfold.com>
 * Purpose:  cycle given given values
 * Input:    id = id of cycle (optional)
 *           values = comma separated list of values to cycle
 *           reset = boolean - resets given var to true
 *			 print = boolean - print var or not. default is true
 *           advance = boolean - whether or not to advance the cycle
 *           delimiter = the value delimiter, default is ","
 *           assign = boolean, assigns to template var instead of
 *                    printed.
 * 
 * Examples: {cycle values="#eeeeee,#d0d0d0d"}
 *           {cycle id=row values="one,two,three" reset=true}
 * -------------------------------------------------------------
 */
function smarty_function_cycle($params, &$smarty)
{
    extract($params);

    if (empty($id)) {
        $id = 'default';
    }

    if (!isset($print)) {
        $print = true;
    }

    if (!isset($advance)) {
        $advance = true;		
    }	
	
    if (!isset($delimiter)) {
        $delimiter = ',';
    }		
	
    if (!in_array('values', array_keys($params))) {
        $smarty->trigger_error("assign: missing 'values' parameter");
        return;
    }
	
	static $cycle_vars;

	$cycle_array = explode($delimiter,$values);
	
	if(!isset($cycle_vars[$id]) || $reset ) {
		$cycle_vars[$id] = 0;
	}

    if (isset($assign)) {
        $print = false;
        $smarty->assign($assign, $cycle_array[$cycle_vars[$id]]);
    }
		
	if($print) {
		echo $cycle_array[$cycle_vars[$id]]."\n";
	}

	if($advance) {
		if ( $cycle_vars[$id] >= count($cycle_array) -1 ) {
			$cycle_vars[$id] = 0;			
		} else {
			$cycle_vars[$id]++;
		}
	}
}

/* vim: set expandtab: */

?>
