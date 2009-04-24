<?php
/**
* Smarty Internal Plugin Compile Template
* 
* Compiles the {template} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Template Class
*/
class Smarty_Internal_Compile_Template extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {template} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return boolean true
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('name');
        $this->optional_attributes = array('_any'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);
        $save = array($_attr, $compiler->template->extracted_compiled_code, $compiler->template->extract_code);
        $this->_open_tag('template', $save);
        $_name = trim($_attr['name'], "'");
        foreach ($_attr as $_key => $_data) {
            $compiler->template->properties['template'][$_name]['parameter'][$_key] = $_data;
        } 
        $compiler->template->extract_code = true;
        $compiler->template->extracted_compiled_code = '';
        $compiler->template->has_code = false;
        return true;
    } 
} 

?>
