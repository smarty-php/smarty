<?php

/**
* Smarty method Clear_All_Assign
* 
* Deletes all assigned Smarty variables at current level
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Delete Smarty variables
* 
* @param object $smarty 
* @param object $data_object object which holds tpl_vars
*/
function  Smarty_Method_Clear_All_Assign($smarty, $data_object = null)
{
    if (isset($data_object)) {
        $ptr = $data_object;
    } else {
        $ptr = $smarty;
    } 
    $ptr->tpl_vars = array();
} 

?>
