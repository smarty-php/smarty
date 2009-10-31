<?php

/**
* Smarty method enableCacheModifyCheck
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
function enableCacheModifyCheck($smarty)
{
    $smarty->cache_modified_check = true;
    return;
} 

?>
