<?php

/**
* Smarty method DisableDefaultTimezone
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
function  Smarty_Method_DisableDefaultTimezone($smarty)
{
    $smarty->set_timezone = false;
    return;
} 

?>
