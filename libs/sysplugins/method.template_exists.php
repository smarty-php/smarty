<?php

/**
* Smarty method Template_Exists
* 
* Checks if a template resource exists
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Checks if a template resource exists
*/

/**
* Check if a template resource exists
* 
* @param string $resource_name template name
* @return boolean status
*/
function template_exists($smarty, $resource_name)
{
    foreach((array)$smarty->template_dir as $_template_dir) {
        $_filepath = $_template_dir . $resource_name;
        if (file_exists($_filepath))
            return true;
    } 
    if (file_exists($resource_name)) return true; 
    // no tpl file found
    return false;
} 

?>
