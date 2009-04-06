<?php

/**
* Smarty method isConfigReadHidden
* 
* is config read hidden mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isConfigReadHidden
* 
* is config read hidden mode
*/
class Smarty_Method_isConfigReadHidden extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->config_read_hidden;
    } 
} 

?>
