<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     modifier
 * Name:     cat
 * Version:  1.0
 * Date:     Feb 24, 2003
 * Author:	 Monte Ohrt <monte@ispi.net>
 * Purpose:  catentate a value to a variable
 * Input:    string to catenate
 * Example:  {$var|cat:"foo"}
 * -------------------------------------------------------------
 */
function smarty_modifier_cat($string, $cat)
{
	return $string . $cat;
}

/* vim: set expandtab: */

?>
