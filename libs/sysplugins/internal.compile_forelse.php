<?php
/**
* Smarty Internal Plugin Compile For Else
* 
* Compiles the {forelse} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Forelse Class
*/
class Smarty_Internal_Compile_Forelse extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {forelse} tag
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

        list($_open_tag, $this->compiler->nocache) = $this->_close_tag(array('for'));
        $this->_open_tag('forelse',array('forelse', $this->compiler->nocache));
        return "<?php }} else { ?>";
    } 
} 

?>
