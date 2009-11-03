<?php

/**
* Smarty method EnableCompileCheck
* 
* Enable compile checking
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable compile checking
*/
function  Smarty_Method_EnableCompileCheck($smarty)
{
    $smarty->compile_check = true;
    return;
} 

?>
