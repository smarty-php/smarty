<?php

/**
* Smarty method Get_Global
* 
* Returns a single or all global variables
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Get_Global
* 
* Returns a single or all global  variables
*/

class Smarty_Method_Get_Global extends Smarty_Internal_Base {
    /**
    * Returns a single or all global  variables
    * 
    * @param string $varname variable name or null
    * @return string variable value or or array of variables
    */
    public function execute($varname = null)
    {
        if (isset($varname)) {
            if (isset($this->smarty->global_tpl_vars[$varname])) {
                return $this->smarty->global_tpl_vars[$varname]->value;
            } else {
                return '';
            } 
        } else {
            $_result = array();
            foreach ($this->smarty->global_tpl_vars AS $key => $var) {
                $_result[$key] = $var->value;
            } 
            return $_result;
        } 
    } 
} 

?>
