<?php

/*======================================================================*\
    Function: smarty_function_escape_special_chars
    Purpose:  used by other smarty functions to escape
	          special chars except for already escaped ones
\*======================================================================*/
function smarty_function_escape_special_chars($string)
{	
	$string = preg_replace('!&(#?\w+);!', '%%%SMARTY_START%%%\\1%%%SMARTY_END%%%', $string);
	$string = htmlspecialchars($string);
	$string = str_replace(array('%%%SMARTY_START%%%','%%%SMARTY_END%%%'), array('&',';'), $string);
	return $string;	
}

/* vim: set expandtab: */

?>
