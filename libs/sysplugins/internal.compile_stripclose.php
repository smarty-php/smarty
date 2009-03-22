<?php
/**
* Smarty Internal Plugin Compile Strip Close
* 
* Compiles the {/strip} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile StripClose Class
*/
class Smarty_Internal_Compile_StripClose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/strip} tag
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

        $saved_attr = $this->_close_tag(array('strip'));

        $_output = "<?php echo preg_replace('![\t ]*[\r\n]+[\t ]*!', '', ob_get_clean()); ?>\n";
        return $_output;
    } 
} 

?>
