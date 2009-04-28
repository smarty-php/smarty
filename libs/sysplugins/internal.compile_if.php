<?php
/**
* Smarty Internal Plugin Compile If
* 
* Compiles the {if} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile If Class
*/
class Smarty_Internal_Compile_If extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {if} tag
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

        $this->_open_tag('if');
        if (is_array($args['if condition'])) {
            $_output = " <?php if (!isset(\$_smarty_tpl->tpl_vars[".$args['if condition']['var']."])) \$_smarty_tpl->tpl_vars[".$args['if condition']['var']."] = new Smarty_Variable;\n";
            $_output .= " if (\$_smarty_tpl->tpl_vars[".$args['if condition']['var']."]->value = ".$args['if condition']['value']."): ?>";
            return $_output;
        } else {
            return '<?php if (' . $args['if condition'] . '): ?>';
        } 
    } 
} 

?>
