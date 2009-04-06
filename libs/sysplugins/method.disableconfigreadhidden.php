<?php

/**
* Smarty method disableConfigReadHidden
* 
* Disable config read hidden mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableConfigReadHidden
* 
* Disable config read hidden mode
*/
class Smarty_Method_disableConfigReadHidden extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->config_read_hidden = false;
        return;
    } 
} 

?>
