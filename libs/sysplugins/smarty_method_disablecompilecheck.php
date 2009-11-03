<?php

/**
* Smarty method DisableCompileCheck
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
function  Smarty_Method_DisableCompileCheck($smarty)
{
    $smarty->compile_check = false;
    return;
} 

?>
