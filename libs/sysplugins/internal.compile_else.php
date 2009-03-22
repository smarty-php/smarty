<?php
/**
* Smarty Internal Plugin Compile Else
* 
* Compiles the {else} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/

/**
* Smarty Internal Plugin Compile Else Class
*/
class Smarty_Internal_Compile_Else extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {else} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler; 
        $this->_close_tag(array('if', 'elseif'));
        $this->_open_tag('else');

        return '<?php else: ?>';
    } 
} 

?>
