<?php
/**
* Smarty Internal Plugin Compile Capture Close
*
* Compiles the {/capture} tag 
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

        $saved_attr = $this->_close_tag(array('capture'));

        $_output = "<?php ";
        if (isset($saved_attr[1])) {
            $_output .= " \$_smarty_tpl->assign($saved_attr[1], ob_get_contents());";
        } 
        $_output .= " \$_smarty_tpl->smarty->_smarty_vars['capture'][$saved_attr[0]]=ob_get_contents();";
        $_output .= " ob_clean(); ?>\n";
        return $_output;
    } 
} 

?>
