<?php

/**
* Smarty method Clear_Cache
* 
* Empties the cache for a specific template
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Clear_Cache
* 
* Empties the cache for a specific template
*/

class Smarty_Method_Clear_Cache extends Smarty_Internal_Base {
    /**
    * Empty cache for a specific template
    * 
    * @param string $template_name template name
    * @param string $cache_id cache id
    * @param string $compile_id compile id
    * @param integer $exp_time expiration time
    * @param string $type resource type
    * @return integer number of cache files deleted
    */
    public function execute($template_name, $cache_id = null, $compile_id = null, $exp_time = null, $type = 'file')
    { 
        // load cache resource
        if (!isset($this->smarty->cache_resource_objects[$type])) {
            $_cache_resource_class = 'Smarty_Internal_CacheResource_' . $type;
            if (!$this->smarty->loadPlugin($_cache_resource_class)) {
                throw new Exception("Undefined cache resource type {$type}");
            } 
            $this->smarty->cache_resource_objects[$type] = new $_cache_resource_class;
        } 

        return $this->smarty->cache_resource_objects[$type]->clear($template_name, $cache_id, $compile_id, $exp_time);
    } 
} 
?>
