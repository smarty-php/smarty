<?php

/**
* Smarty method DisableForceCompile
* 
* Disable forced compiling
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable forced compiling
*/
function  Smarty_Method_DisableForceCompile($smarty)
{
    $smarty->force_compile = false;
    return;
} 

?>
