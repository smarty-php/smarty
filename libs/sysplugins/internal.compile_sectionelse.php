<?php
/**
* Smarty Internal Plugin Compile Section Else
* 
* Compiles the {sectionelse} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Sectionelse Class
*/
class Smarty_Internal_Compile_Sectionelse extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {sectionelse} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler; 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        $this->_close_tag('section');
        $this->_open_tag('sectionelse');
        return "<?php endfor; else: ?>";
    } 
} 

?>
