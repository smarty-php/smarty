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
* Smarty class Get_Config_Vars
* 
* Returns a single or all global config variables
*/

class Smarty_Method_Get_Config_Vars extends Smarty_Internal_Base {
    /**
    * Returns a single or all global config variables
    * 
    * @param string $varname variable name or null
    * @return string variable value or or array of variables
    */
    public function execute($varname = null)
    {
        if (isset($varname)) {
            if (isset($this->smarty->config_vars[$varname])) {
                return $this->smarty->config_vars[$varname];
            } else {
                return '';
            } 
        } else {
            return $this->smarty->config_vars;
        } 
    } 
} 

?>
