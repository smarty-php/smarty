<?php

/**
* Smarty method enableCacheModifyCheck
* 
* Enable cache modify check
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableCacheModifyCheck
* 
* Enable cache modify check
*/
class Smarty_Method_enableCacheModifyCheck extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->cache_modified_check = true;
        return;
    } 
} 

?>
