<?php
/**
* Smarty Internal Plugin Compile Function
* 
* Compiles the {function} {/function} tags
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Function Class
*/
class Smarty_Internal_Compile_Function extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {function} tag
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
        $this->_open_tag('function', $save);
        $_name = trim($_attr['name'], "'");
        foreach ($_attr as $_key => $_data) {
            $compiler->template->properties['function'][$_name]['parameter'][$_key] = $_data;
        } 
        // make function known for recursive calls
        $this->compiler->smarty->template_functions[$_name]['compiled'] = '';
        $compiler->template->extract_code = true;
        $compiler->template->extracted_compiled_code = '';
        $compiler->template->has_code = false;
        return true;
    } 
} 

/**
* Smarty Internal Plugin Compile Functionclose Class
*/
class Smarty_Internal_Compile_Functionclose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/function} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return boolean true
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->compiler->has_code = false; 
        // turn off block code extraction
        $compiler->template->extract_code = false; 
        // check and get attributes
        $this->optional_attributes = array('name');
        $_attr = $this->_get_attributes($args);
        $saved_data = $this->_close_tag(array('function')); 
        // if name does match to opening tag
        if (isset($_attr['name']) && $saved_data[0]['name'] != $_attr['name']) {
            $this->compiler->trigger_template_error('mismatching name attributes "' . $saved_data[0]['name'] . '" and "' . $_attr['name'] . '"');
        } 
        $_name = trim($saved_data[0]['name'], "'");
        $compiler->template->properties['function'][$_name]['compiled'] = str_replace("\n",'_%n',$compiler->template->extracted_compiled_code);
        $this->compiler->smarty->template_functions[$_name]['compiled'] = $compiler->template->extracted_compiled_code;
        $this->compiler->smarty->template_functions[$_name]['parameter'] = $compiler->template->properties['function'][$_name]['parameter'];
        $compiler->template->extracted_compiled_code = $saved_data[1];
        $compiler->template->extract_code = $saved_data[2];
        return true;
    } 
} 

?>
