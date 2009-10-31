<?php

/**
* Smarty method enableConfigReadHidden
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
function enableConfigReadHidden($smarty)
{
    $this->smarty->config_read_hidden = true;
    return;
} 

?>
