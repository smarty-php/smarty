<?php
/**
* Smarty Internal Plugin Compile Block Close
* 
* Compiles the {/capture} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile BlockClose Class
*/
class Smarty_Internal_Compile_BlockClose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/block} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->compiler->has_code = true; 
        // turn off block code extraction
        $compiler->template->extract_code = false; 
        // check and get attributes
        $this->optional_attributes = array('name');
        $_attr = $this->_get_attributes($args);
        $saved_data = $this->_close_tag(array('block')); 
        // if name does match to opening tag
        if (isset($_attr['name']) && $saved_data[0]['name'] != $_attr['name']) {
            $this->compiler->trigger_template_error('mismatching name attributes "' . $saved_data[0]['name'] . '" and "' . $_attr['name'] . '"');
        } 
        $_name = trim($saved_data[0]['name'], "'");
        if (isset($compiler->template->block_data[$_name])) {
            if ($compiler->template->block_data[$_name]['mode'] == 'prepend') {
                $_output = $compiler->template->block_data[$_name]['compiled'] . $compiler->template->extracted_compiled_code;
            } elseif ($compiler->template->block_data[$_name]['mode'] == 'append') {
                $_output = $compiler->template->extracted_compiled_code . $compiler->template->block_data[$_name]['compiled'];
            } elseif (!empty($compiler->template->block_data[$_name])) {
                $_output = $compiler->template->block_data[$_name]['compiled'];
            } 
        } else {
            $_output = $compiler->template->extracted_compiled_code;
        } 
        $compiler->template->extracted_compiled_code = $saved_data[1];
        $compiler->template->extract_code = $saved_data[2];
        return $_output;
    } 
} 

?>
