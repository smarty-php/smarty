<?php

/**
* Smarty method EnableConfigOverwrite
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
function  Smarty_Method_EnableConfigOverwrite($smarty)
{
    $smarty->config_overwrite = true;
    return;
} 

?>
