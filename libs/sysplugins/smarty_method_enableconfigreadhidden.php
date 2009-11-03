<?php

/**
* Smarty method EnableConfigReadHidden
* 
* Enable config read hidden mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable config read hidden mode
*/
function  Smarty_Method_EnableConfigReadHidden($smarty)
{
    $this->smarty->config_read_hidden = true;
    return;
} 

?>
