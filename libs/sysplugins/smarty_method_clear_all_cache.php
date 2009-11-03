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
* Empty cache folder
* 
* @param object $smarty 
* @param integer $exp_time expiration time
* @param string $type resource type
* @return integer number of cache files deleted
*/
function  Smarty_Method_Clear_All_Cache($smarty, $exp_time = null, $type = 'file')
{ 
    // load cache resource
    if (!isset($smarty->cache_resource_objects[$type])) {
        $_cache_resource_class = 'Smarty_Internal_CacheResource_' . $type;
        if (!$smarty->loadPlugin($_cache_resource_class)) {
            throw new Exception("Undefined cache resource type {$type}");
        } 
        $smarty->cache_resource_objects[$type] = new $_cache_resource_class($smarty);
    } 

    return $smarty->cache_resource_objects[$type]->clearAll($exp_time);
} 

?>
