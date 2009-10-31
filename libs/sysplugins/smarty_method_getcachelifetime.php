<?php

/**
* Smarty method getCacheLifetime
* 
* Returns lifetime of cache files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getCacheLifetime
* 
* Returns lifetime of cache files
*/
/**
* Returns lifetime of cache files
* 
* @return integer cache file lifetime
*/
function getCacheLifetime($smarty)
{
    return $smarty->cache_lifetime;
} 

?>
