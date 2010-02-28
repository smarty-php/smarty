<?php
/**
* Smarty Internal Plugin Compile Function Plugin
* 
* Compiles code for the execution of function plugin
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Function Plugin Class
*/
class Smarty_Internal_Compile_Private_Function_Plugin extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the execution of function plugin
    * 
    * @param array $args array with attributes from parser
    * @param string $tag name of function
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler, $tag, $function)
    {
        $this->compiler = $compiler; 
        // This tag does create output
        $this->compiler->has_output = true;

        $this->required_attributes = array();
        $this->optional_attributes = array('_any'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args); 
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
        if (is_array($function)) {
            $output = "<?php echo call_user_func(array('{$function[0]}','{$function[1]}'),{$_params},\$_smarty_tpl->smarty,\$_smarty_tpl);?>\n";
        } else {
            $output = "<?php echo {$function}({$_params},\$_smarty_tpl->smarty,\$_smarty_tpl);?>\n";
        } 
        return $output;
    } 
} 

?>