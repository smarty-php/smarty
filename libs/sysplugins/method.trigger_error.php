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
* Triggers an error meassage
*/

/**
* trigger Smarty error
* 
* @param string $error_msg 
* @param integer $error_type 
*/
function trigger_error($smarty, $error_msg, $error_type = E_USER_WARNING)
{ 
    // trigger_error("Smarty error: $error_msg", $error_type);
    throw new Exception("Smarty error: $error_msg");
} 

?>
