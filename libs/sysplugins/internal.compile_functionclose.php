<?php
/**
* Smarty Internal Plugin Compile Function Close
* 
* Compiles the {/function} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile FunctionClose Class
*/
class Smarty_Internal_Compile_FunctionClose extends Smarty_Internal_CompileBase {
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
        $compiler->template->extracted_compiled_code = $saved_data[1];
        $compiler->template->extract_code = $saved_data[2];
        return true;
    } 
} 

?>
