<?php
/**
* Smarty Internal Plugin Compile Else If
* 
* Compiles the {elseif} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile ElseIf Class
*/
class Smarty_Internal_Compile_ElseIf extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {elseif} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler; 
        $this->required_attributes = array('if condition'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        $this->_close_tag(array('if', 'elseif'));
        $this->_open_tag('elseif');

        return '<?php elseif (' . $args['if condition'] . '): ?>';
    } 
} 

?>
