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
function  Smarty_Method_Clear_All_Cache($smarty, $exp_time = null, $type = null)
{ 
    // load cache resource
    $cacheResource = $smarty->loadCacheResource($type);

    return $cacheResource->clearAll($exp_time);
} 

?>
