<?php

/**
* Smarty method Get_Config_Vars
* 
* Returns a single or all global config variables
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Clear_Config
* 
* Deassigns a single or all global config variables
*/

class Smarty_Method_Clear_Config extends Smarty_Internal_Base {
    /**
* Deassigns a single or all global config variables
    * 
    * @param string $varname variable name or null
    */
    public function execute($varname = null)
    {
        if (isset($varname)) {
        unset($this->smarty->config_vars[$varname]);
            return;
        } else {
            $this->smarty->config_vars = array();
             return;
        } 
    } 
} 

?>
