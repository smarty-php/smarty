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
* Empty cache for a specific template
* 
* @param object $smarty 
* @param string $template_name template name
* @param string $cache_id cache id
* @param string $compile_id compile id
* @param integer $exp_time expiration time
* @param string $type resource type
* @return integer number of cache files deleted
*/
function  Smarty_Method_Clear_Cache($smarty, $template_name, $cache_id = null, $compile_id = null, $exp_time = null, $type = null)
{ 
   // load cache resource
    $cacheResource = $smarty->loadCacheResource($type);

    return $cacheResource->clear($template_name, $cache_id, $compile_id, $exp_time);
} 

?>
