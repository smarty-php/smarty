<?php

/**
* Smarty method isCaching
* 
* is caching
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isCaching
* 
* is caching
*/
class Smarty_Method_isCaching extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->caching;
    } 
} 

?>
