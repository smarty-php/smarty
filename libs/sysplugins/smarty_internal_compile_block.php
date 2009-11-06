<?php
/**
* Smarty Internal Plugin Compile Block
* 
* Compiles the {block}{/block} tags
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
    * @return boolean true
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

/**
* Smarty Internal Plugin Compile BlockClose Class
*/
class Smarty_Internal_Compile_Blockclose extends Smarty_Internal_CompileBase {
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
        $this->smarty = $compiler->smarty;
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
        $_name = trim($saved_data[0]['name'], "\"'");
        if (isset($this->smarty->block_data[$_name])) {
            if (strpos($this->smarty->block_data[$_name]['compiled'], '%%%%SMARTY_PARENT%%%%') !== false) {
                $_output = str_replace('%%%%SMARTY_PARENT%%%%', $compiler->template->extracted_compiled_code, $this->smarty->block_data[$_name]['compiled']);
            } elseif ($this->smarty->block_data[$_name]['mode'] == 'prepend') {
                $_output = $this->smarty->block_data[$_name]['compiled'] . $compiler->template->extracted_compiled_code;
            } elseif ($this->smarty->block_data[$_name]['mode'] == 'append') {
                $_output = $compiler->template->extracted_compiled_code . $this->smarty->block_data[$_name]['compiled'];
            } elseif (!empty($this->smarty->block_data[$_name])) {
                $_output = $this->smarty->block_data[$_name]['compiled'];
            } 
        } else {
            $_output = $compiler->template->extracted_compiled_code;
        } 
        $compiler->template->extracted_compiled_code = $saved_data[1];
        $compiler->template->extract_code = $saved_data[2]; 
        // check for includes in block tags
        preg_match('/(\<\?php \$_smarty_tpl-\>decodeProperties\(\')(.*)(\'.*\?\>)/', $_output, $matches);
        $_output = preg_replace(array('/(\<\?php \$_smarty_tpl-\>decodeProperties\(\')(.*)(\'.*\?\>.*\n)/', '/(\<\?php if\(\!defined\(\'SMARTY_DIR\'\)\))(.*)(\?\>.*\n)/'), '', $_output); 
        if (isset($matches[2])) {
            $prop = unserialize($matches[2]);
            $compiler->template->properties['file_dependency'] = array_merge($compiler->template->properties['file_dependency'], $prop['file_dependency']);
        } 
        return $_output;
    } 
} 

?>
