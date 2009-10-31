<?php

/**
* Smarty method disableDefaultTimezone
* 
* Disable setting of default timezone
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable setting of default timezone
*/
function disableDefaultTimezone($smarty)
{
    $smarty->set_timezone = false;
    return;
} 

?>
