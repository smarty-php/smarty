<?php

/**
* Smarty method Clear_Assign
* 
* Deletes a assigned Smarty variable or array of variables at current level
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Delete a Smarty variable or array of variables
* 
* @param object $smarty 
* @param string $ |array $varname variable name or array of variable names
* @param object $data_object object which holds tpl_vars
*/
function  Smarty_Method_Clear_Assign($smarty, $varname, $data_object = null)
{
    foreach ((array)$varname as $variable) {
        if (isset($data_object)) {
            $ptr = $data_object;
        } else {
            $ptr = $smarty;
        } while ($ptr != null) {
            if (isset($ptr->tpl_vars[$variable])) {
                unset($ptr->tpl_vars[$variable]);
            } 
            $ptr = $ptr->parent;
        } 
    } 
    return;
} 

?>
