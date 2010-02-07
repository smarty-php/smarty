<?php

/**
* Project:     Smarty: the PHP compiling template engine
* File:        smarty_internal_unregister.php
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

class Smarty_Internal_Unregister {
  
    protected $smarty;

    function __construct($smarty) {
      $this->smarty = $smarty;
    }
    
    /**
    * Unregisters block function
    * 
    * @param string $block_tag name of template function
    */
    function block($block_tag)
    {
        if (isset($this->smarty->registered_plugins['block'][$block_tag])) {
            unset($this->smarty->registered_plugins['block'][$block_tag]);
        } 
    }
    
    /**
    * Unregisters compiler function
    * 
    * @param string $compiler_tag name of template function
    */
    function compilerFunction($compiler_tag)
    {
        if (isset($this->smarty->registered_plugins['compiler'][$compiler_tag])) {
            unset($this->smarty->registered_plugins['compiler'][$compiler_tag]);
        } 
    } 
    
    /**
    * Unregisters custom function
    * 
    * @param string $function_tag name of template function
    */
    function templateFunction($function_tag)
    {
        if (isset($this->smarty->registered_plugins['function'][$function_tag])) {
            unset($this->smarty->registered_plugins['function'][$function_tag]);
        } 
    }    
    
    /**
    * Unregisters modifier
    * 
    * @param string $modifier name of template modifier
    */
    function modifier($modifier)
    {
        if (isset($this->smarty->registered_plugins['modifier'][$modifier])) {
            unset($this->smarty->registered_plugins['modifier'][$modifier]);
        } 
    }    
    
    /**
    * Unregisters template object
    * 
    * @param string $object_name name of template object
    */
     function templateObject($object_name)
    {
        unset($this->smarty->registered_objects[$object_name]);
    } 
    
    /**
    * Unregisters an output filter
    * 
    * @param callback $function_name 
    */
    function outputFilter($function_name)
    {
        unset($this->smarty->registered_filters['output'][$this->smarty->_get_filter_name($function_name)]);
    }    
    
    /**
    * Unregisters a postfilter function
    * 
    * @param callback $function_name 
    */
    function postFilter($function_name)
    {
        unset($this->smarty->registered_filters['post'][$this->smarty->_get_filter_name($function_name)]);
    }    
    
    /**
    * Unregisters a prefilter function
    * 
    * @param callback $function_name 
    */
    function preFilter($function_name)
    {
        unset($this->smarty->registered_filters['pre'][$this->smarty->_get_filter_name($function_name)]);
    }    
    
    /**
    * Unregisters a resource
    * 
    * @param string $resource_name name of resource
    */
    function resource($resource_name)
    {
        unset($this->smarty->plugins['resource'][$resource_name]);
    }    
    
    /**
    * Unregisters a variablefilter function
    * 
    * @param callback $function_name 
    */
    function variableFilter($function_name)
    {
        unset($this->smarty->registered_filters['variable'][$this->smarty->_get_filter_name($function_name)]);
    }    
    
}
