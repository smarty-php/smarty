<?php

/**
* Smarty method enableForceCompile
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
public function enableForceCompile($smarty)
{
    $smarty->force_compile = true;
    return;
} 

?>
