<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     counter
 * Purpose:  print out a counter value
 * -------------------------------------------------------------
 */
function smarty_function_counter($params, &$smarty)
{
    static $count = array();
    static $skipval = array();
    static $dir = array();
    static $id = "default";
    static $printval = array();
    static $assign = "";

    extract($params);

    if (!isset($id))
        $id = "default";

    if (isset($start))
        $count[$id] = $start;
    else if (!isset($count[$id]))
        $count[$id]=1;

    if (!isset($print))
        $printval[$id]=true;
    else
        $printval[$id]=$print;
    
    if (!empty($assign)) {
        $printval[$id] = false;
        $smarty->assign($assign, $count[$id]);
    }

    if ($printval[$id])
        echo $count[$id];

    if (isset($skip))
        $skipval[$id] = $skip;
    else if (empty($skipval[$id]))
        $skipval[$id] = 1;
    
    if (isset($direction))
        $dir[$id] = $direction;
    else if (!isset($dir[$id]))
        $dir[$id] = "up";

    if ($dir[$id] == "down")
        $count[$id] -= $skipval[$id];
    else
        $count[$id] += $skipval[$id];
}

/* vim: set expandtab: */

?>
