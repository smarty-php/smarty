<?php

/**
* Smarty method Unregister_Block
* 
* Unregister a Smarty block function plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Unregister_Block
* 
* Unregister a Smarty block function plugin
*/

class Smarty_Method_Unregister_Block extends Smarty_Internal_Base {
    /**
    * Unregisters block function
    * 
    * @param string $block_tag name of template function
    */
    public function execute($block_tag)
    {
        if (isset($this->smarty->registered_plugins[$block_tag]) && $this->smarty->registered_plugins[$block_tag][0] == 'block') {
            unset($this->smarty->registered_plugins[$block_tag]);
        } 
    } 
} 

?>
