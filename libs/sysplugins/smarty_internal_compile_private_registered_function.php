<?php
/**
* Smarty Internal Plugin Compile Registered Function
* 
* Compiles code for the execution of a registered function
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Registered Function Class
*/
class Smarty_Internal_Compile_Private_Registered_Function extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the execution of a registered function
    * 
    * @param array $args array with attributes from parser
    * @param string $tag name of function
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler, $tag)
    {
        $this->compiler = $compiler; 
        // This tag does create output
        $this->compiler->has_output = true;

        $this->required_attributes = array();
        $this->optional_attributes = array('_any'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args); 
        // not cachable?
        $this->compiler->tag_nocache = !$compiler->smarty->registered_plugins['function'][$tag][1]; 
        // convert attributes into parameter array string
        $_paramsArray = array();
        foreach ($_attr as $_key => $_value) {
            if (is_int($_key)) {
                $_paramsArray[] = "$_key=>$_value";
            } else {
                $_paramsArray[] = "'$_key'=>$_value";
            } 
        } 
        $_params = 'array(' . implode(",", $_paramsArray) . ')'; 
        // compile code
        $output = '<?php echo call_user_func_array($_smarty_tpl->smarty->registered_plugins[\'function\'][\'' . $tag . '\'][0],array(' . $_params . ',$_smarty_tpl->smarty,$_smarty_tpl));?>';
        return $output;
    } 
} 

?>
