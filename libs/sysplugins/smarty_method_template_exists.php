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
function  Smarty_Method_Template_Exists($smarty, $resource_name)
{ 
    // create template object
    $tpl = new $smarty->template_class($resource_name, $smarty); 
    // check if it does exists 
        return $tpl->isExisting();
} 

?>
