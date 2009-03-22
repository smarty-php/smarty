<?php
/**
* Smarty Internal Plugin Compile Capture
*
* Compiles the {capture} tag 
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
        $this->optional_attributes = array('name', 'assign'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        if (isset($_attr['name']))
            $buffer = $_attr['name'];
        else
            $buffer = "'default'";

        if (isset($_attr['assign']))
            $assign = $_attr['assign'];
        else
            $assign = null;
       
        $this->_open_tag('capture',array($buffer, $assign));

        $_output = "<?php ob_start(); ?>";

        return $_output;
    } 
} 

?>
