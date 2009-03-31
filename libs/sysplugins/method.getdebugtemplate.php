<?php

/**
* Smarty method getDebugTemplate
* 
* Returns debug template filepath
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getDebugTemplate
* 
* Returns debug template filepath
*/

class Smarty_Method_GetDebugTemplate extends Smarty_Internal_Base {
    /**
    * Returns directory of cache files
    * 
    * @return string debug template filepath
    */
    public function execute()
    {
        return $this->smarty->debug_tpl;
    } 
} 

?>
