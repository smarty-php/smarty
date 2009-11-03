<?php

/**
* Smarty method DisableDebugging
* 
* Disable debugging
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable debugging
*/
function  Smarty_Method_DisableDebugging($smarty)
{
    $smarty->debugging = false;
    return;
} 
?>
