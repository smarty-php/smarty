<?php

/**
* Smarty method Trigger_Error
* 
* Triggers an error meassage
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Trigger_Error
* 
* Triggers an error meassage
*/

class Smarty_Method_Trigger_Error extends Smarty_Internal_Base {
    /**
    * trigger Smarty error
    * 
    * @param string $error_msg 
    * @param integer $error_type 
    */
    public function execute($error_msg, $error_type = E_USER_WARNING)
    { 
        // trigger_error("Smarty error: $error_msg", $error_type);
        throw new Exception("Smarty error: $error_msg");
    } 
} 

?>
