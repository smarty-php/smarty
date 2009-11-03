<?php

/**
* Smarty method IsDefaultTimezone
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
function  Smarty_Method_IsDefaultTimezone($smarty)
{
    return $smarty->set_timezone = false;
} 

?>
