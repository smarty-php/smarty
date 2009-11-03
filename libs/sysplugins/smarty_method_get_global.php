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
* Returns a single or all global  variables
* 
* @param object $smarty 
* @param string $varname variable name or null
* @return string variable value or or array of variables
*/
function  Smarty_Method_Get_Global($smarty, $varname = null)
{
    if (isset($varname)) {
        if (isset($smarty->global_tpl_vars[$varname])) {
            return $smarty->global_tpl_vars[$varname]->value;
        } else {
            return '';
        } 
    } else {
        $_result = array();
        foreach ($smarty->global_tpl_vars AS $key => $var) {
            $_result[$key] = $var->value;
        } 
        return $_result;
    } 
} 

?>
