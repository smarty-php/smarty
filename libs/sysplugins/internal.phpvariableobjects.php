<?php

/**
* Smarty Internal Plugin PHPVariableObjects
* 
* Classes and methods to extract Smarty variables to PHP variable objects
* 
* @package Smarty
* @subpackage Templates
* @author Uwe Tews 
*/

class Smarty_Internal_PHPVariableObjects {
    /**
    * generate PHP variable object from Smarty variables for use in PHP templates
    * 
    * @param array $data nested array with data values
    * @return array nested array of variable objects
    */
    static function createPHPVarObj ($data)
    {
        if (is_array($data)) {
            $_result = array();
            foreach ($data as $key => $value) {
                if (is_array($value)) {
                    $_result[$key] = self::createPHPVarObj($value);
                } else {
                    $_result[$key] = new PHP_Variable_Object ($value);
                } 
            } 
            return $_result;
        } else {
            return new PHP_Variable_Object ($data);
        } 
    } 
} 

/**
* class for the PHP variable object
* 
* This class defines the PHP variable object
* and contains the __tostring method to return the value
* and the __call method to execute chained methods for 
* object variables and modifiers
*/
class PHP_Variable_Object {
    // template variable
    public $value;
    /**
    * create PHP variable object
    * 
    * @param mixed $value the value to assign
    */
    public function __construct ($value, $funcFlag = true)
    {
        $this->value = $value;
        if ($funcFlag) {
            $this->funcFlag = true;
        } 
    } 

    /**
    * Return output string
    * 
    * @return string variable content
    */
    public function __toString()
    {
        if (isset($this->_tmp)) {
            // result from modifer
            $_tmp = $this->_tmp; 
            // must unset because variable could be reused
            unset($this->_tmp);
            return (string)$_tmp;
        } else {
            // variable value
            return (string)$this->value;
        } 
        if (isset($this->funcFlag)) $this->funcFlag = true;
    } 

    /**
    * Methode chaining on object methods and modifier
    * 
    * Lazy loads modifier if required
    * 
    * @return object variable object
    */
    public function __call($name, $args = array())
    {
        if (is_object($this->value)) {
            if (method_exists($this->value, $name) || $this->funcFlag) {
                if (isset($this->funcFlag)) $this->funcFlag = false; 
                // call objects methode
                $_tmp = call_user_func_array(array($this->value, $name), $args);
                if (is_object($_tmp)) {
                    // is methode chaining, we must return the variable object
                    return $this;
                } else {
                    // save result and return variable object
                    $this->_tmp = $_tmp;
                    return $this;
                } 
            } 
        } 
        $_smarty = Smarty::instance(); 
        // get variable value
        if (isset($this->_tmp)) {
            $args = array_merge(array($this->_tmp), $args);
        } else {
            $args = array_merge(array($this->value), $args);
        } 
        // call modifier and save result
        if (is_callable($name)) {
            $this->_tmp = call_user_func_array($name, $args);
        } else {
            $this->_tmp = call_user_func_array(array($_smarty->plugin_handler, $name), array($args, 'modifier'));
        } 
        // return variable object for methode chaining
        return $this;
    } 
} 

/**
* class for PHP function handling
*/
class PHP_Function_Handler {
    // template object
    public $template = null;

    public function __construct($tpl)
    {
        $this->smarty = Smarty::instance();
        $this->template = $tpl;
    } 
    /**
    * calls PHP function from PHP template
    * 
    * @param string $name function name
    * @param array $args function arguments
    * @return unkown function result
    */
    public function __call($name, $args)
    {
        if (function_exists($name)) {
            // test security
            if (!$this->template->security || empty($this->smarty->security_policy->php_functions) || in_array($name, $this->smarty->security_policy->php_functions)) {
                // use PHP function if found
                return call_user_func_array($name, $args);
            } else {
                throw new Exception ("PHP function \"" . $name . "\" not allowed by security setting");
            } 
        } 
        // nothing found, throw exception
        throw new Exception("Unkown function {$name}");
    } 
} 

?>
