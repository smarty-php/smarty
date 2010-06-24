<?php

/**
 * Project:     Smarty: the PHP compiling template engine
 * File:        smarty_internal_register.php
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

class Smarty_Internal_Register {
    protected $smarty;

    function __construct($smarty)
    {
        $this->smarty = $smarty;
    } 

    /**
     * Registers block function to be used in templates
     * 
     * @param string $block_tag name of template block
     * @param string $block_impl PHP function to register
     * @param boolean $cacheable if true (default) this fuction is cachable
     * @param array $cache_attr caching attributes if any
     */
    function block($block_tag, $block_impl, $cacheable = true, $cache_attr = array())
    {
        if (isset($this->smarty->registered_plugins['block'][$block_tag])) {
            throw new Exception("Plugin tag \"{$block_tag}\" already registered");
        } elseif (!is_callable($block_impl)) {
            throw new Exception("Plugin \"{$block_tag}\" not callable");
        } else {
            $this->smarty->registered_plugins['block'][$block_tag] =
            array($block_impl, $cacheable, $cache_attr);
        } 
    } 

    /**
     * Registers compiler function
     * 
     * @param string $compiler_tag of template function
     * @param string $compiler_impl name of PHP function to register
     * @param boolean $cacheable if true (default) this fuction is cachable
     */
    function compilerFunction($compiler_tag, $compiler_impl, $cacheable = true)
    {
        if (isset($this->smarty->registered_plugins['compiler'][$compiler_tag])) {
            throw new Exception("Plugin tag \"{$compiler_tag}\" already registered");
        } elseif (!is_callable($compiler_impl)) {
            throw new Exception("Plugin \"{$compiler_tag}\" not callable");
        } else {
            $this->smarty->registered_plugins['compiler'][$compiler_tag] =
            array($compiler_impl, $cacheable);
        } 
    } 

    /**
     * Registers custom function to be used in templates
     * 
     * @param string $function_tag the name of the template function
     * @param string $function_impl the name of the PHP function to register
     * @param boolean $cacheable if true (default) this fuction is cachable
     * @param array $cache_attr caching attributes if any
     */
    function templateFunction($function_tag, $function_impl, $cacheable = true, $cache_attr = array())
    {
        if (isset($this->smarty->registered_plugins['function'][$function_tag])) {
            throw new Exception("Plugin tag \"{$function_tag}\" already registered");
        } elseif (!is_callable($function_impl)) {
            throw new Exception("Plugin \"{$function_tag}\" not callable");
        } else {
            $this->smarty->registered_plugins['function'][$function_tag] =
            array($function_impl, $cacheable, $cache_attr);
        } 
    } 

    /**
     * Registers modifier to be used in templates
     * 
     * @param string $modifier_name name of template modifier
     * @param string $modifier_impl name of PHP function to register
     */
    function modifier($modifier_name, $modifier_impl)
    {
        if (isset($this->smarty->registered_plugins['modifier'][$modifier_name])) {
            throw new Exception("Plugin \"{$modifier_name}\" already registered");
        } elseif (!is_callable($modifier_impl)) {
            throw new Exception("Plugin \"{$modifier_name}\" not callable");
        } else {
            $this->smarty->registered_plugins['modifier'][$modifier_name] =
            array($modifier_impl);
        } 
    } 

    /**
     * Registers object to be used in templates
     * 
     * @param string $object name of template object
     * @param object $ &$object_impl the referenced PHP object to register
     * @param mixed $ null | array $allowed list of allowed methods (empty = all)
     * @param boolean $smarty_args smarty argument format, else traditional
     * @param mixed $ null | array $block_functs list of methods that are block format
     */
    function templateObject($object_name, $object_impl, $allowed = array(), $smarty_args = true, $block_methods = array())
    { 
        // test if allowed methodes callable
        if (!empty($allowed)) {
            foreach ((array)$allowed as $method) {
                if (!is_callable(array($object_impl, $method))) {
                    throw new Exception("Undefined method '$method' in registered object");
                } 
            } 
        } 
        // test if block methodes callable
        if (!empty($block_methods)) {
            foreach ((array)$block_methods as $method) {
                if (!is_callable(array($object_impl, $method))) {
                    throw new Exception("Undefined method '$method' in registered object");
                } 
            } 
        } 
        // register the object
        $this->smarty->registered_objects[$object_name] =
        array($object_impl, (array)$allowed, (boolean)$smarty_args, (array)$block_methods);
    } 

    /**
     * Registers static classes to be used in templates
     * 
     * @param string $class name of template class
     * @param string $class_impl the referenced PHP class to register
     */
    function templateClass($class_name, $class_impl)
    { 
        // test if exists
        if (!class_exists($class_impl)) {
            throw new Exception("Undefined class '$class_impl' in register template class");
        } 
        // register the class
        $this->smarty->registered_classes[$class_name] = $class_impl;
    } 

    /**
     * Registers an output filter function to apply
     * to a template output
     * 
     * @param callback $function_name 
     */
    function outputFilter($function_name)
    {
        $this->smarty->registered_filters['output'][$this->smarty->_get_filter_name($function_name)] = $function_name;
    } 

    /**
     * Registers a postfilter function to apply
     * to a compiled template after compilation
     * 
     * @param callback $function_name 
     */
    function postFilter($function_name)
    {
        $this->smarty->registered_filters['post'][$this->smarty->_get_filter_name($function_name)] = $function_name;
    } 

    /**
     * Registers a prefilter function to apply
     * to a template before compiling
     * 
     * @param callback $function_name 
     */
    function preFilter($function_name)
    {
        $this->smarty->registered_filters['pre'][$this->smarty->_get_filter_name($function_name)] = $function_name;
    } 

    /**
     * Registers a resource to fetch a template
     * 
     * @param string $resource_type name of resource type
     * @param array $function_names array of functions to handle resource
     */
    function resource($resource_type, $function_names)
    {
        if (count($function_names) == 4) {
            $this->smarty->_plugins['resource'][$resource_type] =
            array($function_names, false);
        } elseif (count($function_names) == 5) {
            $this->smarty->_plugins['resource'][$resource_type] =
            array(array(array(&$function_names[0], $function_names[1]),
                    array(&$function_names[0], $function_names[2]),
                    array(&$function_names[0], $function_names[3]),
                    array(&$function_names[0], $function_names[4])),
                false);
        } else {
            throw new Exception("malformed function-list for '$resource_type' in register_resource");
        } 
    } 

    /**
     * Registers an output filter function which
     * runs over any variable output
     * 
     * @param callback $function_name 
     */
    function variableFilter($function_name)
    {
        $this->smarty->registered_filters['variable'][$this->smarty->_get_filter_name($function_name)] = $function_name;
    } 

    /**
     * Registers a default plugin handler
     * 
     * @param  $function_name mixed string | array $plugin class/methode name
     */
    function defaultPluginHandler($function_name)
    {
        if (is_callable($function_name)) {
            $this->smarty->default_plugin_handler_func = $function_name;
        } else {
            throw new Exception("Default plugin handler '$function_name' not callable");
        } 
    } 

    /**
     * Registers a default template handler
     * 
     * @param  $function_name mixed string | array class/method name
     */
    function defaultTemplateHandler($function_name)
    {
        if (is_callable($function_name)) {
            $this->smarty->default_template_handler_func = $function_name;
        } else {
            throw new Exception("Default template handler '$function_name' not callable");
        } 
    } 
}
