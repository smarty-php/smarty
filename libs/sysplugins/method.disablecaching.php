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
* Smarty class disableCaching
* 
* Disable caching
*/
class Smarty_Method_DisableCaching extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->caching = false;
    } 
} 

?>
