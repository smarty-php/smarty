<?php

/**
* Smarty method disableCaching
* 
* Disable caching
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable caching
*/
function DisableCaching()
{
    $this->smarty->caching = false;
    return;
} 

?>
