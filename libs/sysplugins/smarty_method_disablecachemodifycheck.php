<?php

/**
* Smarty method DisableCacheModifyCheck
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
function  Smarty_Method_DisableCacheModifyCheck($smarty)
    {
        $smarty->cache_modified_check = false;
        return ;
    } 

?>
