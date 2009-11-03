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
* Deassigns a single or all global config variables
* 
* @param object $smarty 
* @param string $varname variable name or null
*/
function  Smarty_Method_Clear_Config($smarty, $varname = null)
{
    if (isset($varname)) {
        unset($smarty->config_vars[$varname]);
        return;
    } else {
        $smarty->config_vars = array();
        return;
    } 
} 

?>
