<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     cycle
 * Version:  1.2
 * Date:     May 3, 2002
 * Author:	 Monte Ohrt <monte@ispi.net>
 * Credits:  Mark Priatel <mpriatel@rogers.com>
 *           Gerard <gerard@interfold.com>
 * Purpose:  cycle through given values
 * Input:    id = id of cycle (optional)
 *           values = comma separated list of values to cycle
 *                    (this can be left out for subsequent calls)
 *           reset = boolean - resets given var to true
 *			 print = boolean - print var or not. default is true
 *           advance = boolean - whether or not to advance the cycle
 *           delimiter = the value delimiter, default is ","
 *           assign = boolean, assigns to template var instead of
 *                    printed.
 * 
 * Examples: {cycle values="#eeeeee,#d0d0d0d"}
 *           {cycle id=row values="one,two,three" reset=true}
 *           {cycle id=row}
 * -------------------------------------------------------------
 */
function smarty_function_cycle($params, &$smarty)
{
	static $cycle_vars;
	
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
		if(!isset($cycle_vars[$id]['values'])) {
        	$smarty->trigger_error("cycle: missing 'values' parameter");
        	return;
		}
    } else {
		if(isset($cycle_vars[$id]['values'])
			&& $cycle_vars[$id]['values'] != $values ) {
			$cycle_vars[$id]['index'] = 0;
		}
		$cycle_vars[$id]['values'] = $values;
	}

	$cycle_array = explode($delimiter,$cycle_vars[$id]['values']);
	
	if(!isset($cycle_vars[$id]['index']) || $reset ) {
		$cycle_vars[$id]['index'] = 0;
	}

    if (isset($assign)) {
        $print = false;
        $smarty->assign($assign, $cycle_array[$cycle_vars[$id]['index']]);
    }
		
	if($print) {
		echo $cycle_array[$cycle_vars[$id]['index']]."\n";
	}

	if($advance) {
		if ( $cycle_vars[$id]['index'] >= count($cycle_array) -1 ) {
			$cycle_vars[$id]['index'] = 0;			
		} else {
			$cycle_vars[$id]['index']++;
		}
	}
}

/* vim: set expandtab: */

?>
