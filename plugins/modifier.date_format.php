<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     modifier
 * Name:     date_format
 * Purpose:  format datestamps via strftime
 * -------------------------------------------------------------
 */
require_once SMARTY_DIR . $this->plugins_dir . '/shared.make_timestamp.php';
function smarty_modifier_date_format($string, $format="%b %e, %Y")
{
    return strftime($format, smarty_make_timestamp($string));
}

/* vim: set expandtab: */

?>
