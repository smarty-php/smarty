<?php

/**
* Smarty Internal Plugin Compile TemplateCall
* 
* Compiles the {templatecall} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile TemplateCall Class
*/
class Smarty_Internal_Compile_TemplateCall extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {templateall} tag
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
        $_name = trim( $_attr['name'],"'");
        // create template object
        $_output = "<?php \$_template = new Smarty_Template ('string:', \$_smarty_tpl);"; 
        // assign default paramter
        if (isset($compiler->template->properties['template'][$_name]['parameter'])) {
            foreach ($compiler->template->properties['template'][$_name]['parameter'] as $_key => $_value) {
                if (!isset($_attr[$_key])) {
                    $_output .= "\$_template->assign('$_key',$_value);";
                } 
            } 
        } 
        // delete {include} standard attributes
        unset($_attr['name'], $_attr['assign']); 
        // remaining attributes must be assigned as smarty variable
        if (!empty($_attr)) {
            // create variables
            foreach ($_attr as $_key => $_value) {
                $_output .= "\$_template->assign('$_key',$_value);";
            } 
        }
        if (isset($compiler->template->properties['template'][$_name]['compiled'])) {
            $_compiled = str_replace(array('_%n',"'"), array('',"\'"), $compiler->template->properties['template'][$_name]['compiled']);
            $_output .= "\$_template->compiled_template = '$_compiled'; \$_template->mustCompile = false;";
        } else {
// for recursion
            $_output .= "\$_template->compiled_template = \$_smarty_tpl->compiled_template; \$_template->mustCompile = false;";
        }
        // was there an assign attribute
        if (isset($_assign)) {
            $_output .= "\$_smarty_tpl->assign($_assign,\$_smarty_tpl->smarty->fetch(\$_template)); ?>";
        } else {
            $_output .= "echo \$_smarty_tpl->smarty->fetch(\$_template); ?>";
        } 
        return $_output;
    } 
} 

?>
