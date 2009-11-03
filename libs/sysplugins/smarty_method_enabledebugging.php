<?php

/**
* Smarty method EnableDebugging
* 
* Enable debugging
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable debugging
*/
function  Smarty_Method_EnableDebugging($smarty)
{
    $this->smarty->debugging = true;
    return;
} 

?>
