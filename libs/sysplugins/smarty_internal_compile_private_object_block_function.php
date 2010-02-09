<?php
/**
* Smarty Internal Plugin Compile Object Block Function
* 
* Compiles code for registered objects as block function
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Object Block Function Class
*/
class Smarty_Internal_Compile_Private_Object_Block_Function extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the execution of block plugin
    * 
    * @param array $args array with attributes from parser
    * @param string $tag name of block function
    * @param string $methode name of methode to call
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler, $tag, $methode)
    {
        $this->compiler = $compiler;
        if (strlen($tag) < 5 || substr_compare($tag, 'close', -5, 5) != 0) {
            // opening tag of block plugin
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

            $this->_open_tag($tag . '->' . $methode, $_params); 
            // compile code
            $output = "<?php \$_smarty_tpl->smarty->_tag_stack[] = array('{$tag}->{$methode}', {$_params}); \$_block_repeat=true; \$_smarty_tpl->smarty->registered_objects['{$tag}'][0]->{$methode}({$_params}, null, \$_smarty_tpl->smarty, \$_block_repeat, \$_smarty_tpl);while (\$_block_repeat) { ob_start();?>";
        } else {
            $base_tag = substr($tag, 0, -5); 
            // closing tag of block plugin
            $_params = $this->_close_tag($base_tag . '->' . $methode); 
            // This tag does create output
            $this->compiler->has_output = true; 
            // compile code
            $output = "<?php \$_block_content = ob_get_contents(); ob_end_clean(); \$_block_repeat=false; echo \$_smarty_tpl->smarty->registered_objects['{$base_tag}'][0]->{$methode}({$_params}, \$_block_content, \$_smarty_tpl->smarty, \$_block_repeat, \$_smarty_tpl); } array_pop(\$_smarty_tpl->smarty->_tag_stack);?>";
        } 
        return $output."\n";
    } 
} 

?>
