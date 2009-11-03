<?php

/**
* Smarty method EnableCacheModifyCheck
* 
* Enable cache modify check
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable cache modify check
*/
function  Smarty_Method_EnableCacheModifyCheck($smarty)
{
    $smarty->cache_modified_check = true;
    return;
} 

?>
