<?php

/**
* Smarty Internal Plugin Compile Append
* 
* Compiles the {append} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Append Class
*/
class Smarty_Internal_Compile_Append extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {append} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('var', 'value');
        $this->optional_attributes = array('scope', 'nocache', 'index');

        $_nocache = 'null';
        $_scope = 'null'; 
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

        if (isset($_attr['scope'])) {
            if ($_attr['scope'] == '\'parent\'') {
                $_scope = SMARTY_PARENT_SCOPE;
            } elseif ($_attr['scope'] == '\'root\'') {
                $_scope = SMARTY_ROOT_SCOPE;
           } elseif ($_attr['scope'] == '\'global\'') {
                $_scope = SMARTY_GLOBAL_SCOPE;
            } 
        } 
        // compiled output
        if (isset($_attr['index'])) {
            return "<?php \$_smarty_tpl->append($_attr[var],array($_attr[index] => $_attr[value]),true,$_nocache,$_scope);?>";
        } else {
            return "<?php \$_smarty_tpl->append($_attr[var],$_attr[value],false,$_nocache,$_scope);?>";
        } 
    } 
} 

?>
