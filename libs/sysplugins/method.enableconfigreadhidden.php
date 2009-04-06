<?php

/**
* Smarty method enableConfigReadHidden
* 
* Enable config read hidden mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableConfigReadHidden
* 
* Enable config read hidden mode
*/
class Smarty_Method_enableConfigReadHidden extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->config_read_hidden = true;
        return;
    } 
} 

?>
