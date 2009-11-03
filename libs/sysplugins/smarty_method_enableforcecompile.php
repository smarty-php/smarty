<?php

/**
* Smarty method EnableForceCompile
* 
* Enable forced compiling
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable forced compiling
*/
function  Smarty_Method_EnableForceCompile($smarty)
{
    $smarty->force_compile = true;
    return;
} 

?>
