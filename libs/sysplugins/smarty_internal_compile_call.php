<?php

/**
* Smarty Internal Plugin Compile Function_Call
* 
* Compiles the calls of user defined tags defined by {function}
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Function_Call Class
*/
class Smarty_Internal_Compile_Call extends Smarty_Internal_CompileBase {
    /**
    * Compiles the calls of user defined tags defined by {function}
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('name');
        $this->optional_attributes = array('_any'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args); 
        // save posible attributes
        if (isset($_attr['assign'])) {
            // output will be stored in a smarty variable instead of beind displayed
            $_assign = $_attr['assign'];
        } 
        // set flag (compiled code of {function} must be included in cache file
        if ($this->compiler->nocache || $this->compiler->tag_nocache) {
            $nocache = 'true';
        } else {
            $nocache = 'false';
        } 
        // create template object
        $_output = "<?php \$_template = new Smarty_Internal_Function_Call_Handler ({$_attr['name']}, \$_smarty_tpl->smarty, \$_smarty_tpl, {$nocache});\n"; 
        // delete {include} standard attributes
        unset($_attr['name'], $_attr['assign']); 
        // remaining attributes must be assigned as smarty variable
        if (!empty($_attr)) {
            // create variables
            foreach ($_attr as $_key => $_value) {
                $_output .= "\$_template->assign('$_key',$_value);\n";
            } 
        } 
        // was there an assign attribute
        if (isset($_assign)) {
            $_output .= "\$_smarty_tpl->assign({$_assign},\$_template->getRenderedTemplate());\n";
        } else {
            $_output .= "echo \$_template->getRenderedTemplate();\n";
        } 
        $_output .= 'unset($_template);?>';
        return $_output;
    } 
} 

?>
