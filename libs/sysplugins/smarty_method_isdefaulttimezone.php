<?php

/**
* Smarty method isDefaultTimezone
* 
* is setting of default timezone
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* is setting of default timezone
*/
function isDefaultTimezone($smarty)
{
    return $smarty->set_timezone = false;
} 

?>
