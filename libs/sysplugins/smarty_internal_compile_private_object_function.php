<?php
/**
* Smarty Internal Plugin Compile Object Funtion
* 
* Compiles code for registered objects as function
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Object Function Class
*/
class Smarty_Internal_Compile_Private_Object_Function extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the execution of function plugin
    * 
    * @param array $args array with attributes from parser
    * @param string $tag name of function
    * @param string $methode name of methode to call
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler, $tag, $methode)
    {
        $this->compiler = $compiler; 
        // This tag does create output
        $this->compiler->has_output = true;

        $this->required_attributes = array();
        $this->optional_attributes = array('_any'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args); 
        // convert attributes into parameter array string
        if ($this->compiler->smarty->registered_objects[$tag][2]) {
            $_paramsArray = array();
            foreach ($_attr as $_key => $_value) {
                if (is_int($_key)) {
                    $_paramsArray[] = "$_key=>$_value";
                } else {
                    $_paramsArray[] = "'$_key'=>$_value";
                } 
            } 
            $_params = 'array(' . implode(",", $_paramsArray) . ')';
            $output = "<?php echo \$_smarty_tpl->smarty->registered_objects['{$tag}'][0]->{$methode}({$_params},\$_smarty_tpl->smarty,\$_smarty_tpl);?>";
        } else {
            $_params = implode(",", $_attr);
            $output = "<?php echo \$_smarty_tpl->smarty->registered_objects['{$tag}'][0]->{$methode}({$_params});?>";
        } 
        return $output;
    } 
} 

?>
