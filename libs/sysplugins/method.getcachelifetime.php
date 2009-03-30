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
class Smarty_Method_GetCacheLifetime extends Smarty_Internal_Base {
    /**
* Returns lifetime of cache files
    * 
    * @return integer cache file lifetime
    */
    public function execute()
    {
        return $this->smarty->cache_lifetime;
    } 
} 

?>
