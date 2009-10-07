<?php
/**
* Smarty Internal Plugin Compile Capture
* 
* Compiles the {capture} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Capture Class
*/
class Smarty_Internal_Compile_Capture extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {capture} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->optional_attributes = array('name', 'assign', 'append'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        $buffer = isset($_attr['name']) ? $_attr['name'] : "'default'";
        $assign = isset($_attr['assign']) ? $_attr['assign'] : null;
        $append = isset($_attr['append']) ? $_attr['append'] : null;

        $this->compiler->_capture_stack[] = array($buffer, $assign, $append);

        $_output = "<?php ob_start(); ?>";

        return $_output;
    } 
} 

?>
