<?php 
/**
 * Smarty Internal Plugin Compile Debug
 *
 * Compiles the {debug} tag 
 * It opens a window the the Smarty Debugging Console
 * @package Smarty
 * @subpackage Compiler
 * @author Uwe Tews
 */

/**
 * Smarty Internal Plugin Compile Debug Class
 */ 
class Smarty_Internal_Compile_Debug extends Smarty_Internal_CompileBase {
    /**
     * Compiles code for the {debug} tag
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

		// compile always as nocache
		$this->compiler->tag_nocache = true;

        // display debug template
        $_output = "<?php Smarty_Internal_Plugin_Loader::loadPlugin('Smarty_Internal_Debug', \$_smarty_tpl->smarty->plugins_dir); Smarty_Internal_Debug::display_debug(\$_smarty_tpl->smarty); ?>";
        return $_output;
    } 
} 

?>