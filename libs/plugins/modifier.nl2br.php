<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     modifier
 * Name:     nl2br
 * Version:  1.0
 * Date:     Feb 26, 2003
 * Author:	 Monte Ohrt <monte@ispi.net>
 * Purpose:  convert \r\n, \r or \n to <br />
 * Input:    contents = contents to replace
 *           preceed_test = if true, includes preceeding break tags
 *           in replacement
 * Example:  {$text|nl2br}
 * -------------------------------------------------------------
 */
function smarty_modifier_nl2br($string)
{
    return nl2br($string);
}

/* vim: set expandtab: */

?>
