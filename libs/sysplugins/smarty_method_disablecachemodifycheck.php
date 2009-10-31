<?php

/**
* Smarty method disableCacheModifyCheck
* 
* Disable cache modify check
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* 
* Disable cache modify check
*/
function disableCacheModifyCheck($smarty)
    {
        $smarty->cache_modified_check = false;
        return ;
    } 

?>
