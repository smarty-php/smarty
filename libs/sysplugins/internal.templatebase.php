<?php

/**
* Smarty Internal Plugin TemplateBase
* 
* This file contains the basic classes and methodes for template and variable creation
* 
* @package Smarty
* @subpackage Templates
* @author Uwe Tews 
*/

/**
* Base class with template and variable methodes
*/
class Smarty_Internal_TemplateBase {
    /**
    * assigns a Smarty variable
    * 
    * @param array $ |string $tpl_var the template variable name(s)
    * @param mixed $value the value to assign
    * @param boolean $nocache if true any output of this variable will be not cached
    * @param boolean $scope the scope the variable will have  (local,parent or root)
    */
    public function assign($tpl_var, $value = null, $nocache = false, $scope = SMARTY_LOCAL_SCOPE)
    {
        if (is_array($tpl_var)) {
            foreach ($tpl_var as $_key => $_val) {
                if ($_key != '') {
                    $this->check_tplvar($_key);
                    $this->tpl_vars[$_key] = new Smarty_variable($_val, $nocache, $scope);
                } 
            } 
        } else {
            if ($tpl_var != '') {
                $this->check_tplvar($tpl_var);
                $this->tpl_vars[$tpl_var] = new Smarty_variable($value, $nocache, $scope);
            } 
        } 
    } 
    /**
    * assigns a global Smarty variable
    * 
    * @param string $varname the global variable name
    * @param mixed $value the value to assign
    * @param boolean $nocache if true any output of this variable will be not cached
    */
    public function assign_global($varname, $value = null, $nocache = false)
    {
        $_ptr = Smarty::instance();
        if ($varname != '') {
            $this->check_tplvar($varname);
            $_ptr->global_tpl_vars[$varname] = new Smarty_variable($value, $nocache);
        } 
    } 
    /**
    * assigns values to template variables by reference
    * 
    * @param string $tpl_var the template variable name
    * @param mixed $ &$value the referenced value to assign
    * @param boolean $nocache if true any output of this variable will be not cached
    * @param boolean $scope the scope the variable will have  (local,parent or root)
    */
    public function assign_by_ref($tpl_var, &$value, $nocache = false, $scope = SMARTY_LOCAL_SCOPE)
    {
        if ($tpl_var != '') {
            $this->check_tplvar($tpl_var);
            $this->tpl_vars[$tpl_var] = new Smarty_variable(null, $nocache, $scope);
            $this->tpl_vars[$tpl_var]->value = &$value;
        } 
    } 
    /**
    * appends values to template variables
    * 
    * @param array $ |string $tpl_var the template variable name(s)
    * @param mixed $value the value to append
    * @param boolean $merge flag if array elements shall be merged
    * @param boolean $nocache if true any output of this variable will be not cached
    * @param boolean $scope the scope the variable will have  (local,parent or root)
    */
    public function append($tpl_var, $value = null, $merge = false, $nocache = false, $scope = SMARTY_LOCAL_SCOPE)
    {
        if (is_array($tpl_var)) {
            // $tpl_var is an array, ignore $value
            foreach ($tpl_var as $_key => $_val) {
                if ($_key != '') {
                    if (!isset($this->tpl_vars[$_key])) {
                        $this->check_tplvar($_key);
                        $tpl_var_inst = $this->getVariable($_key, null, true, false);
                        if ($tpl_var_inst instanceof Undefined_Smarty_Variable) {
                            $this->tpl_vars[$_key] = new Smarty_variable(null, $nocache, $scope);
                        } else {
                            $this->tpl_vars[$_key] = clone $tpl_var_inst;
                            if ($scope != SMARTY_LOCAL_SCOPE) {
                                $this->tpl_vars[$_key]->scope = $scope;
                            } 
                        } 
                    } 
                    if (!(is_array($this->tpl_vars[$_key]->value) || $this->tpl_vars[$_key]->value instanceof ArrayAccess)) {
                        settype($this->tpl_vars[$_key]->value, 'array');
                    } 
                    if ($merge && is_array($_val)) {
                        foreach($_val as $_mkey => $_mval) {
                            $this->tpl_vars[$_key]->value[$_mkey] = $_mval;
                        } 
                    } else {
                        $this->tpl_vars[$_key]->value[] = $_val;
                    } 
                } 
            } 
        } else {
            if ($tpl_var != '' && isset($value)) {
                if (!isset($this->tpl_vars[$tpl_var])) {
                    $this->check_tplvar($tpl_var);
                    $tpl_var_inst = $this->getVariable($tpl_var, null, true, false);
                    if ($tpl_var_inst instanceof Undefined_Smarty_Variable) {
                        $this->tpl_vars[$tpl_var] = new Smarty_variable(null, $nocache, $scope);
                    } else {
                        $this->tpl_vars[$tpl_var] = clone $tpl_var_inst;
                        if ($scope != SMARTY_LOCAL_SCOPE) {
                            $this->tpl_vars[$tpl_var]->scope = $scope;
                        } 
                    } 
                } 
                if (!(is_array($this->tpl_vars[$tpl_var]->value) || $this->tpl_vars[$tpl_var]->value instanceof ArrayAccess)) {
                    settype($this->tpl_vars[$tpl_var]->value, 'array');
                } 
                if ($merge && is_array($value)) {
                    foreach($value as $_mkey => $_mval) {
                        $this->tpl_vars[$tpl_var]->value[$_mkey] = $_mval;
                    } 
                } else {
                    $this->tpl_vars[$tpl_var]->value[] = $value;
                } 
            } 
        } 
    } 

    /**
    * appends values to template variables by reference
    * 
    * @param string $tpl_var the template variable name
    * @param mixed $ &$value the referenced value to append
    * @param boolean $merge flag if array elements shall be merged
    */
    public function append_by_ref($tpl_var, &$value, $merge = false)
    {
        if ($tpl_var != '' && isset($value)) {
            if (!isset($this->tpl_vars[$tpl_var])) {
                $this->tpl_vars[$tpl_var] = new Smarty_variable();
            } 
            if (!@is_array($this->tpl_vars[$tpl_var]->value)) {
                settype($this->tpl_vars[$tpl_var]->value, 'array');
            } 
            if ($merge && is_array($value)) {
                foreach($value as $_key => $_val) {
                    $this->tpl_vars[$tpl_var]->value[$_key] = &$value[$_key];
                } 
            } else {
                $this->tpl_vars[$tpl_var]->value[] = &$value;
            } 
        } 
    } 

    /**
    * check if template variable name is reserved.
    * 
    * @param string $tpl_var the template variable
    */
    private function check_tplvar($tpl_var)
    {
        if (in_array($tpl_var, array('this', 'smarty'))) {
            throw new Exception("Cannot assign value to reserved var '{$tpl_var}'");
        } 
    } 

    /**
    * clear the given assigned template variable.
    * 
    * @param string $ |array $tpl_var the template variable(s) to clear
    */
    public function clear_assign($tpl_var)
    {
        if (is_array($tpl_var)) {
            foreach ($tpl_var as $curr_var) {
                unset($this->tpl_vars[$curr_var]);
            } 
        } else {
            unset($this->tpl_vars[$tpl_var]);
        } 
    } 

    /**
    * clear all the assigned template variables.
    */
    public function clear_all_assign()
    {
        $this->tpl_vars = array();
    } 

    /**
    * gets the object of a Smarty variable
    * 
    * @param string $variable the name of the Smarty variable
    * @param object $_ptr optional pointer to data object
    * @param boolean $search_parents search also in parent data
    * @return object the object of the variable
    */
    public function getVariable($variable, $_ptr = null, $search_parents = true, $error_enable = true)
    {
        if ($_ptr === null) {
            $_ptr = $this;
        } while ($_ptr !== null) {
            if (isset($_ptr->tpl_vars[$variable])) {
                // found it, return it
                return $_ptr->tpl_vars[$variable];
            } 
            // not found, try at parent
            if ($search_parents) {
                $_ptr = $_ptr->parent;
            } else {
                $_ptr = null;
            } 
        } 
        $_ptr = Smarty::instance();
        if (isset($_ptr->global_tpl_vars[$variable])) {
            // found it, return it
            return $_ptr->global_tpl_vars[$variable];
        } 
        if (Smarty::$error_unassigned && $error_enable) {
            throw new Exception('Undefined Smarty variable "' . $variable . '"');
        } else {
            return new Undefined_Smarty_Variable;
        } 
    } 
    /**
    * gets  a config variable
    * 
    * @param string $variable the name of the config variable
    * @return mixed the value of the config variable
    */
    public function getConfigVariable($variable)
    {
        $_ptr = $this;
        while ($_ptr !== null) {
            if (isset($_ptr->config_vars[$variable])) {
                // found it, return it
                return $_ptr->config_vars[$variable];
            } 
            // not found, try at parent
            $_ptr = $_ptr->parent;
        } 
        if (Smarty::$error_unassigned) {
            throw new Exception('Undefined config variable "' . $variable . '"');
        } else {
            return '';
        } 
    } 
    /**
    * gets  a global variable
    * 
    * @param string $variable the name of the global variable
    * @return mixed the value of the global variable
    */
    public function getGlobalVariable($variable)
    {
        $_ptr = Smarty::instance();
        if (isset($_ptr->global_tpl_vars[$variable])) {
            // found it, return it
            return $_ptr->global_tpl_vars[$variable];
        } 
        if (Smarty::$error_unassigned) {
            throw new Exception('Undefined global variable "' . $variable . '"');
        } else {
            return '';
        } 
    } 
    /**
    * gets  a stream variable
    * 
    * @param string $variable the stream of the variable
    * @return mixed the value of the global variable
    */
    public function getStreamVariable($variable)
    {
        $_result = '';
        if ($fp = fopen($variable, 'r+')) {
            while (!feof($fp)) {
                $_result .= fgets($fp);
            } 
            fclose($fp);
            return $_result;
        } 

        if (Smarty::$error_unassigned) {
            throw new Exception('Undefined global variable "' . $variable . '"');
        } else {
            return '';
        } 
    } 

    /**
    * creates a template object
    * 
    * @param string $template the resource handle of the template file
    * @param object $parent next higher level of Smarty variables
    * @param mixed $cache_id cache id to be used with this template
    * @param mixed $compile_id compile id to be used with this template
    * @returns object template object
    */
    public function createTemplate($template, $parent = null, $cache_id = null, $compile_id = null)
    {
        if (!is_object($template)) {
            // we got a template resource
            $_templateId = $this->buildTemplateId ($template, $cache_id, $compile_id); 
            // already in template cache?
            if (isset(Smarty::$template_objects[$_templateId])) {
                // return cached template object
                return Smarty::$template_objects[$_templateId];
            } else {
                // create and cache new template object
                return new Smarty_Internal_Template ($template, $parent, $cache_id, $compile_id);
            } 
        } else {
            // just return a copy of template class
            return $template;
        } 
    } 

    /**
    * generates a template id
    * 
    * @param string $_resource the resource handle of the template file
    * @param mixed $_cache_id cache id to be used with this template
    * @param mixed $_compile_id compile id to be used with this template
    * @returns string a unique template id
    */
    public function buildTemplateId ($_resource, $_cache_id, $_compile_id)
    { 
        // return md5($_resource . md5($_cache_id) . md5($_compile_id));
        return crc32($_resource . $_cache_id . $_compile_id);
    } 

    /**
    * return current time
    * 
    * @returns double current time
    */
    function _get_time()
    {
        $_mtime = microtime();
        $_mtime = explode(" ", $_mtime);
        return (double)($_mtime[1]) + (double)($_mtime[0]);
    } 
} 

/**
* class for the Smarty data object
* 
* The Smarty data object will hold Smarty variables in the current scope
* 
* @param object $parent tpl_vars next higher level of Smarty variables
*/
class Smarty_Data extends Smarty_Internal_TemplateBase {
    // array of variable objects
    public $tpl_vars = array(); 
    // back pointer to parent object
    public $parent = null; 
    // config vars
    public $config_vars = array();
    /**
    * create Smarty data object
    */
    public function __construct ($_parent = null)
    {
        if (is_object($_parent)) {
            // when object set up back pointer
            $this->parent = $_parent;
        } elseif (is_array($_parent)) {
            // set up variable values
            foreach ($_parent as $_key => $_val) {
                $this->tpl_vars[$_key] = new Smarty_variable($_val);
            } 
        } else {
            throw new Exception("Wrong type for template variables");
        } 
    } 
} 
/**
* class for the Smarty variable object
* 
* This class defines the Smarty variable object
*/
class Smarty_Variable {
    // template variable
    public $value;
    public $nocache;
    public $scope;
    /**
    * create Smarty variable object
    * 
    * @param mixed $value the value to assign
    * @param boolean $nocache if true any output of this variable will be not cached
    * @param boolean $scope the scope the variable will have  (local,parent or root)
    */
    public function __construct ($value = null, $nocache = false, $scope = SMARTY_LOCAL_SCOPE)
    {
        $this->value = $value;
        $this->nocache = $nocache;
        $this->scope = $scope;
    } 
} 

/**
* class for undefined variable object
* 
* This class defines an object for undefined variable handling
*/
class Undefined_Smarty_Variable {
    // return always false
    public function __get ($name)
    {
        if ($name == 'nocache') {
            return false;
        } else {
            return null;
        } 
    } 
} 

?>
