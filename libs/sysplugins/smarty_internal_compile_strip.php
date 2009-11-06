<?php
/**
* Smarty Internal Plugin Compile Strip
* 
* Compiles the {strip} {/strip} tags
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Strip Class
*/
class Smarty_Internal_Compile_Strip extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {strip} tag
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

        $this->_open_tag('strip');

        $_output = "<?php ob_start(); ?>";

        return $_output;
    } 
} 

/**
* Smarty Internal Plugin Compile Stripclose Class
*/
class Smarty_Internal_Compile_Stripclose extends Smarty_Internal_CompileBase {
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
