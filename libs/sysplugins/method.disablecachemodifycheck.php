<?php

/**
* Smarty method disableCacheModifyCheck
* 
* Disable cache modify check
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableCacheModifyCheck
* 
* Disable cache modify check
*/
class Smarty_Method_disableCacheModifyCheck extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->cache_modified_check = false;
        return ;
    } 
} 

?>
