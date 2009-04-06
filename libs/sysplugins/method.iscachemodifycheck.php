<?php

/**
* Smarty method isCacheModifyCheck
* 
* is cache modify check
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isCacheModifyCheck
* 
* is cache modify check
*/
class Smarty_Method_isCacheModifyCheck extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->cache_modified_check;
    } 
} 

?>
