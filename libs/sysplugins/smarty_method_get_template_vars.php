<?php

/**
* Smarty method Get_Template_Vars
* 
* Returns a single or all template variables
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Returns a single or all template variables
*/

/**
* Returns a single or all template variables
* 
* @param string $varname variable name or null
* @return string variable value or or array of variables
*/
function  Smarty_Method_Get_Template_Vars($smarty, $varname = null, $_ptr = null, $search_parents = true)
{
    if (isset($varname)) {
        $_var = $smarty->getVariable($varname, $_ptr, $search_parents);
        if (is_object($_var)) {
            return $_var->value;
        } else {
            return null;
        } 
    } else {
        $_result = array();
        if ($_ptr === null) {
            $_ptr = $smarty;
        } while ($_ptr !== null) {
            foreach ($_ptr->tpl_vars AS $key => $var) {
                $_result[$key] = $var->value;
            } 
            // not found, try at parent
            if ($search_parents) {
                $_ptr = $_ptr->parent;
            } else {
                $_ptr = null;
            } 
        } 
        if ($search_parents) {
            foreach ($smarty->global_tpl_vars AS $key => $var) {
                $_result[$key] = $var->value;
            } 
        } 
        return $_result;
    } 
} 

?>
