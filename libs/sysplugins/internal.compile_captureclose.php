<?php
/**
* Smarty Internal Plugin Compile Capture Close
* 
* Compiles the {/capture} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile CaptureClose Class
*/
class Smarty_Internal_Compile_CaptureClose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/capture} tag
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

        list($buffer, $assign, $append) = array_pop($this->compiler->_capture_stack);

        $_output = "<?php ";
        if (isset($assign)) {
            $_output .= " \$_smarty_tpl->assign($assign, ob_get_contents());";
        } 
        if (isset($append)) {
            $_output .= " \$_smarty_tpl->append($append, ob_get_contents());";
        } 
        $_output .= " \$_smarty_tpl->smarty->_smarty_vars['capture'][$buffer]=ob_get_clean(); ?>\n";
        return $_output;
    } 
} 

?>
