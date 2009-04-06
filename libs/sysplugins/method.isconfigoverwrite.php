<?php

/**
* Smarty method isConfigOverwrite
* 
* is config overwrite mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isConfigOverwrite
* 
* is config overwrite mode
*/
class Smarty_Method_isConfigOverwrite extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->config_overwrite;
        
    } 
} 

?>
