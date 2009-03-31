<?php

/**
* Smarty method setDebugTemplate
* 
* Sets debug template filepath
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class setDebugTemplate
* 
* Sets debug template filepath
*/

class Smarty_Method_SetDebugTemplate extends Smarty_Internal_Base {
    /**
    * Sets debug template filepath
    * 
    * @param string $ array debug template filepath
    */
    public function execute($debug_tpl)
    {
        $this->smarty->debug_tpl = $debug_tpl;
        return;
    } 
} 

?>
