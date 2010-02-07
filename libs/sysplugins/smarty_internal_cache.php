<?php

/**
* Project:     Smarty: the PHP compiling template engine
* File:        smarty_internal_cache.php
* SVN:         $Id: $
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
* 
* For questions, help, comments, discussion, etc., please join the
* Smarty mailing list. Send a blank e-mail to
* smarty-discussion-subscribe@googlegroups.com
* 
* @link http://www.smarty.net/
* @copyright 2008 New Digital Group, Inc.
* @author Monte Ohrt <monte at ohrt dot com> 
* @author Uwe Tews 
* @package Smarty
* @subpackage PluginsInternal
* @version 3-SVN$Rev: 3286 $
*/

class Smarty_Internal_Cache {
  
    protected $smarty;

    function __construct($smarty) {
      $this->smarty = $smarty;
    }

    /**
    * Loads cache resource.
    * 
    * @return object of cache resource
    */
    public function loadResource($type = null) {
        if (!isset($type)) {
            $type = $this->smarty->caching_type;
        } 
        // already loaded?
        if (isset($this->smarty->cache_resource_objects[$type])) {
            return $this->smarty->cache_resource_objects[$type];
        } 
        if (in_array($type, $this->smarty->cache_resource_types)) {
            $cache_resource_class = 'Smarty_Internal_CacheResource_' . ucfirst($type);
            return $this->smarty->cache_resource_objects[$type] = new $cache_resource_class($this->smarty);
        } 
        else {
            // try plugins dir
            $cache_resource_class = 'Smarty_CacheResource_' . ucfirst($type);
            if ($this->smarty->loadPlugin($cache_resource_class)) {
                return $this->smarty->cache_resource_objects[$type] = new $cache_resource_class($this->smarty);
            } 
            else {
                throw new Exception("Unable to load cache resource '{$type}'");
            } 
        } 
    } 

    /**
    * Empty cache folder
    * 
    * @param integer $exp_time expiration time
    * @param string $type resource type
    * @return integer number of cache files deleted
    */
    function clearAll($exp_time = null, $type = null)
    { 
        return $this->loadResource($type)->clearAll($exp_time);
    }        

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
    function clear($template_name, $cache_id = null, $compile_id = null, $exp_time = null, $type = null)
    { 
       // load cache resource
        $cacheResource = $this->loadResource($type);
    
        return $cacheResource->clear($template_name, $cache_id, $compile_id, $exp_time);
    }
    
}
