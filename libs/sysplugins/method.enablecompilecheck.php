<?php

/**
* Smarty method enableCompileCheck
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
function enableCompileCheck($smarty)
{
    $smarty->compile_check = true;
    return;
} 

?>
