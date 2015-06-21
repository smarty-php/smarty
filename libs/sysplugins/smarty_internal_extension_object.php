<?php

/**
 * Smarty Extension Object
 *
 * Register object methods
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Extension_Object
{
    /**
     * Registers object to be used in templates
     *
     * @param  \Smarty_Internal_Template|\Smarty $obj
     * @param  string                            $object_name
     * @param  object                            $object_impl   the referenced PHP object to register
     * @param  array                             $allowed       list of allowed methods (empty = all)
     * @param  boolean                           $smarty_args   smarty argument format, else traditional
     * @param  array                             $block_methods list of block-methods
     *
     * @return \Smarty_Internal_Templatebase
     * @throws \SmartyException
     */
    static function registerObject($obj, $object_name, $object_impl, $allowed = array(), $smarty_args = true, $block_methods = array())
    {
        // test if allowed methods callable
        if (!empty($allowed)) {
            foreach ((array) $allowed as $method) {
                if (!is_callable(array($object_impl, $method)) && !property_exists($object_impl, $method)) {
                    throw new SmartyException("Undefined method or property '$method' in registered object");
                }
            }
        }
        // test if block methods callable
        if (!empty($block_methods)) {
            foreach ((array) $block_methods as $method) {
                if (!is_callable(array($object_impl, $method))) {
                    throw new SmartyException("Undefined method '$method' in registered object");
                }
            }
        }
        // register the object
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        $smarty->registered_objects[$object_name] = array($object_impl, (array) $allowed, (boolean) $smarty_args,
            (array) $block_methods);
    }

    /**
     * return a reference to a registered object
     *
     * @param  \Smarty_Internal_Template|\Smarty $obj
     * @param  string                            $name object name
     *
     * @return object
     * @throws \SmartyException if no such object is found
     */
    static function getRegisteredObject($obj, $name)
    {
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        if (!isset($smarty->registered_objects[$name])) {
            throw new SmartyException("'$name' is not a registered object");
        }
        if (!is_object($smarty->registered_objects[$name][0])) {
            throw new SmartyException("registered '$name' is not an object");
        }
        return $smarty->registered_objects[$name][0];
    }

    /**
     * unregister an object
     *
     * @param  \Smarty_Internal_Template|\Smarty $obj
     * @param  string                            $name object name
     *
     * @return \Smarty_Internal_Templatebase current Smarty_Internal_Templatebase (or Smarty or
     *                                      Smarty_Internal_Template) instance for chaining
     */
    static function unregisterObject($obj, $name)
    {
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        if (isset($smarty->registered_objects[$name])) {
            unset($smarty->registered_objects[$name]);
        }
    }

    /**
     * Registers static classes to be used in templates
     *
     * @param  \Smarty_Internal_Template|\Smarty $obj
     * @param  string                            $class_name
     * @param  string                            $class_impl the referenced PHP class to register
     *
     * @return \Smarty_Internal_Templatebase
     * @throws \SmartyException
     */
    static function registerClass($obj, $class_name, $class_impl)
    {
        // test if exists
        if (!class_exists($class_impl)) {
            throw new SmartyException("Undefined class '$class_impl' in register template class");
        }
        // register the class
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        $smarty->registered_classes[$class_name] = $class_impl;
    }

}