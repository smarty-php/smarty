<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     modifier
 * Name:     date_format
 * Purpose:  format datestamps via strftime
 * Input:    string: input date string
 *           format: strftime format for output
 *           default_date: default date if $string is empty
 * -------------------------------------------------------------
 */
require_once SMARTY_DIR . $this->plugins_dir . '/shared.make_timestamp.php';
function smarty_modifier_date_format($string, $format="%b %e, %Y", $default_date=null)
{
	if($string != '') {
    	return strftime($format, smarty_make_timestamp($string));
	} elseif (isset($default_date) && $default_date != '') {		
    	return strftime($format, smarty_make_timestamp($default_date));
	} else {
		return;
	}
}

/* vim: set expandtab: */

?>
