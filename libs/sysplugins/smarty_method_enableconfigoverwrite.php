<?php

/**
* Smarty method enableConfigOverwrite
* 
* Enable config overwrite mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable config overwrite mode
*/
function enableConfigOverwrite($smarty)
{
    $smarty->config_overwrite = true;
    return;
} 

?>
