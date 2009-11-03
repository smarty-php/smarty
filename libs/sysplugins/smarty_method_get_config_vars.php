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
* Returns a single or all global config variables
*/

/**
* Returns a single or all global config variables
* 
* @param string $varname variable name or null
* @return string variable value or or array of variables
*/
function  Smarty_Method_Get_Config_Vars($smarty, $varname = null)
{
    if (isset($varname)) {
        if (isset($smarty->config_vars[$varname])) {
            return $smarty->config_vars[$varname];
        } else {
            return '';
        } 
    } else {
        return $smarty->config_vars;
    } 
} 

?>
