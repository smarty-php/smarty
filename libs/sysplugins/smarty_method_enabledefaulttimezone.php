<?php

/**
* Smarty method enableDefaultTimezone
* 
* Enable setting of default timezone
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable setting of default timezone
*/
function enableDefaultTimezone($smarty)
{
    $this->smarty->set_timezone = true;
    return;
} 

?>
