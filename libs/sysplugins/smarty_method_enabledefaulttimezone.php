<?php

/**
* Smarty method EnableDefaultTimezone
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
function  Smarty_Method_EnableDefaultTimezone($smarty)
{
    $this->smarty->set_timezone = true;
    return;
} 

?>
