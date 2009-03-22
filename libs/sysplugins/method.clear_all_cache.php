<?php

/**
* Smarty method Clear_All_Cache
* 
* Empties the cache folder
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Clear_All_Cache
* 
* Empties the cache folder
*/

class Smarty_Method_Clear_All_Cache extends Smarty_Internal_Base {
    /**
    * Empty cache folder
    * 
    * @param integer $exp_time expiration time
    * @param string $type resource type
    * @return integer number of cache files deleted
    */
    public function execute($exp_time = null, $type = 'file')
    { 
        // load cache resource
        if (!isset($this->smarty->cache_resource_objects[$type])) {
            $_cache_resource_class = 'Smarty_Internal_CacheResource_' . $type;
            if (!$this->smarty->loadPlugin($_cache_resource_class)) {
                throw new Exception("Undefined cache resource type {$type}");
            } 
            $this->smarty->cache_resource_objects[$type] = new $_cache_resource_class;
        } 

        return $this->smarty->cache_resource_objects[$type]->clearAll($exp_time);
    } 
} 
?>
