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
* Smarty class Clear_All_Assign
* 
* Delete Smarty variables at current level
*/

class Smarty_Method_Clear_All_Assign extends Smarty_Internal_Base {
    /**
    * Delete Smarty variables
    * @param object $data_object object which holds tpl_vars
    */
    public function execute($data_object = null)
    {
        if (isset($data_object)) {
            $ptr = $data_object;
        } else {
            $ptr = $this->smarty;
        } 
        $ptr->tpl_vars = array();
    } 
} 

?>
