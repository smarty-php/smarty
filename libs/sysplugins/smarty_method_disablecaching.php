<?php

/**
* Smarty method DisableCaching
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
function  Smarty_Method_DisableCaching()
{
    $this->smarty->caching = false;
    return;
} 

?>
