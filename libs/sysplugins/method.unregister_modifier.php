<?php

/**
* Smarty method Unregister_Modifier
* 
* Unregister a Smarty modifier plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Unregister_Modifier
* 
* Unregister a Smarty modifier plugin
*/

class Smarty_Method_Unregister_Modifier extends Smarty_Internal_Base {
    /**
    * Unregisters modifier
    * 
    * @param string $modifier name of template modifier
    */
    public function execute($modifier)
    {
        if (isset($this->smarty->registered_plugins[$modifier]) && $this->smarty->registered_plugins[$modifier][0] == 'modifier') {
            unset($this->smarty->registered_plugins[$modifier]);
        } 
    } 
} 

?>
