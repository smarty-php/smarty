<?php
/**
* Smarty Internal Plugin Compile Block Plugin
* 
* Compiles code for the execution of block plugin
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Block Plugin Class
*/
class Smarty_Internal_Compile_Block_Plugin extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the execution of block plugin
    * 
    * @param array $args array with attributes from parser
    * @param string $tag name of block function
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $tag, $compiler)
    {
        $this->compiler = $compiler; 
        if (strlen($tag) < 6 || substr_compare($tag, 'close', -5, 5) != 0) {
            // opening tag of block plugin
            $this->required_attributes = array();
            $this->optional_attributes = array('_any'); 

            // check and get attributes
            $_attr = $this->_get_attributes($args);

            // convert attributes into parameter array string
            $_paramsArray = array();
            foreach ($_attr as $_key => $_value) {
                $_paramsArray[] = "'$_key'=>$_value";
            } 
            $_params = 'array(' . implode(",", $_paramsArray) . ')'; 
            
            $this->_open_tag($tag, $_params);

            // compile code
            $output = '<?php $_block_repeat=true;$_smarty_tpl->smarty->plugin_handler->' . $tag . '(array(' . $_params . ', null, $_smarty_tpl->smarty, $_block_repeat, $_smarty_tpl),\'block\');while ($_block_repeat) { ob_start();?>';
        } else {
            // closing tag of block plugin
            $_params = $this->_close_tag(substr($tag,0,-5));
            // This tag does create output
            $this->compiler->has_output = true;
            // compile code
            $output = '<?php $_block_content = ob_get_contents(); ob_end_clean(); $_block_repeat=false; echo $_smarty_tpl->smarty->plugin_handler->' . substr($tag,0,-5) . '(array(' . $_params . ', $_block_content, $_smarty_tpl->smarty, $_block_repeat, $_smarty_tpl),\'block\'); }?>';
        } 
        return $output;
    } 
} 

?>
