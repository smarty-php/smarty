<?php

/**
* Smarty method getCacheDir
* 
* Returns directory of cache files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getCacheDir
* 
* Returns directory of cache files
*/

class Smarty_Method_GetCacheDir extends Smarty_Internal_Base {
    /**
    * Returns directory of cache files
    * 
    * @return array cache folder
    */
    public function execute()
    {
        return $this->smarty->cache_dir;
    } 
} 

?>
