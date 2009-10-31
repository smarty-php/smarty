<?php

/**
* Smarty method disableDebugging
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
function disableDebugging($smarty)
{
    $smarty->debugging = false;
    return;
} 
?>
