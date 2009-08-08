<?php

/**
* Smarty method disableConfigReadHidden
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
function disableConfigReadHidden ($smarty)
{
    $this->smarty->config_read_hidden = false;
    return;
} 
?>
