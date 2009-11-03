<?php

/**
* Smarty method DisableConfigReadHidden
* 
* Disable config read hidden mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable config read hidden mode
*/
function  Smarty_Method_DisableConfigReadHidden ($smarty)
{
    $this->smarty->config_read_hidden = false;
    return;
} 
?>
