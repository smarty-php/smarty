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
* Smarty class Template_Exists
* 
* Checks if a template resource exists
*/

class Smarty_Method_Template_Exists extends Smarty_Internal_Base {
    /**
    * Check if a template resource exists
    * 
    * @param string $resource_name template name
    * @return boolean status
    */ 
    public function execute($resource_name)
    {
        foreach((array)$this->smarty->template_dir as $_template_dir) {
            $_filepath = $_template_dir . $resource_name;
            if (file_exists($_filepath))
                return true;
        } 
        if (file_exists($resource_name)) return true; 
        // no tpl file found
        return false;
    } 
} 

?>
