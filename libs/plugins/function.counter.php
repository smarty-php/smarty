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
    static $counters = array();

    extract($params);

    if (!isset($name)) {
		if(isset($id)) {
			$name = $id;
		} else {		
        	$name = "default";
		}
	}

    if (!isset($counters[$name])) {
        $counters[$name] = array(
            'start'=>1,
            'skip'=>1,
            'assign'=>null,
            'direction'=>'up'
            );
    }
    $counter =& $counters[$name];


    if (isset($start))
        $counter['start'] = $start;
    else if (!isset($counter['start']))
        $counter['start'] = 1;

    if (!isset($counter['count']))
        $counter['count'] = $counter['start'];

    if (!empty($assign)) {
        $counter['assign'] = $assign;
    }

    if (!empty($counter['assign'])) {
        $smarty->assign($counter['assign'], $counter['count']);
    }
    
    if (!isset($print))
        $print = empty($counter['assign']);
    else
        $print = (bool)$print;

    if (isset($skip)) {
        $counter['skip'] = $skip;
    }
    
    if ($print) {
        $retval = $counter['count'];
	} else {
		$retval = null;
	}

    if (isset($direction)) {
        $counter['direction'] = $direction;
    }

    if ($counter['direction'] == "down")
        $counter['count'] -= $counter['skip'];
    else
        $counter['count'] += $counter['skip'];
	
	return $retval;
	
}

/* vim: set expandtab: */

?>
