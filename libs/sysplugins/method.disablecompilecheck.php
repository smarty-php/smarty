<?php

/**
* Smarty method disableCompileCheck
* 
* Disable compile checking
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable compile checking
* 
* @param object $smarty 
*/
function DisableCompileCheck($smarty)
{
    $smarty->compile_check = false;
    return;
} 

?>
