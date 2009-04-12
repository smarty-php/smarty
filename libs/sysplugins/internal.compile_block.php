<?php
/**
* Smarty Internal Plugin Compile Capture
* 
* Compiles the {block} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Block Class
*/
class Smarty_Internal_Compile_Block extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {block} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('name');
        $this->optional_attributes = array('assign'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);
        $save = array($_attr, $compiler->template->extracted_compiled_code, $compiler->template->extract_code); 
        $this->_open_tag('block', $save);
        $compiler->template->extract_code = true;
        $compiler->template->extracted_compiled_code = '';
        $compiler->template->has_code = false; 
        return true;
    } 
} 

?>
