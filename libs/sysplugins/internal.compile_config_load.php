<?php

/**
* Smarty Internal Plugin Compile Config Load
* 
* Compiles the {config load} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Config Load Class
*/
class Smarty_Internal_Compile_Config_Load extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {config_load} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('file');
        $this->optional_attributes = array('section', 'scope'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args); 
        // save posible attributes
        $conf_file = $_attr['file'];
        if (isset($_attr['section'])) {
            $section = $_attr['section'];
        } else {
            $section = 'null';
        } 
        $scope = '$_smarty_tpl->smarty';
        if (isset($_attr['scope'])) {
            if ($_attr['scope'] == '\'local\'') {
                $scope = '$_smarty_tpl';
            } elseif ($_attr['scope'] == '\'parent\'') {
                $scope = '$_smarty_tpl->parent';
            } 
        } 

        // create config object
        $_output = "<?php  \$_config = new Smarty_Internal_Config($conf_file, \$_smarty_tpl->smarty, \$_smarty_tpl);";
        $_output .= "\$_config->loadConfigVars($section, $scope); ?>";
        return $_output;
    } 
} 

?>
