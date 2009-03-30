<?php

/**
* Smarty method getTemplateDir
* 
* Returns template directory
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getTemplate_dir
* 
* Returns template directory
*/

class Smarty_Method_GetTemplateDir extends Smarty_Internal_Base {
    /**
    * Returns template directory
    * 
    * @return array template folders
    */
    public function execute()
    {
        return $this->smarty->template_dir;
    } 
} 

?>
