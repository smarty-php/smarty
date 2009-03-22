<?php

/**
* Smarty Internal Base
* 
* @package Smarty
* @subpackage PluginsInternal
* @author Monte Ohrt
*/
/**
/**
* Smarty Internal Base Class
*/
abstract class Smarty_Internal_Base {
    /**
    * Set up instance of Smarty object
    */
    function __construct()
    {
        $this->smarty = Smarty::instance();
    } 
} 

?>
