<?php

/**
* Smarty Internal Plugin Compile Assign
* 
* Compiles the {assign} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Assign Global Class
*/
class Smarty_Internal_Compile_Assign_Global extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {assign_global} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('var', 'value');
        $this->optional_attributes = array('nocache');

        $_nocache = 'false';
        // check for nocache attribute before _get_attributes because
        // it shall not controll caching of the compiled code, but is a parameter
        if (isset($args['nocache'])) {
            if ($args['nocache'] == 'true') {
                $_nocache = 'true';
                $_nocache_boolean = true;
            } 
            unset($args['nocache']);
        } 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        // compiled output
        return "<?php \$_smarty_tpl->smarty->assign_global($_attr[var],$_attr[value],$_nocache);?>";
    } 
} 

?>
