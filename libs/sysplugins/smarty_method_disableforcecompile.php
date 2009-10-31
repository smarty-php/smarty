<?php

/**
* Smarty method disableForceCompile
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
function disableForceCompile($smarty)
{
    $smarty->force_compile = false;
    return;
} 

?>
