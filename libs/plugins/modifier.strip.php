<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     modifier
 * Name:     strip
 * Purpose:  Replace all repeated spaces, newlines, tabs
 *           with a single space or supplied replacement string.
 * Example:  {$var|strip} {$var|strip:"&nbsp;"}
 * Author:   Monte Ohrt <monte@ispi.net>
 * Version:  1.0
 * Date:     September 25th, 2002
 * -------------------------------------------------------------
 */
function smarty_modifier_strip($text, $replace = ' ')
{
	return preg_replace('!\s+!', $replace, $text);
}

/* vim: set expandtab: */

?>
