<?php
/**
* Smarty Internal Plugin Compile Else If
* 
* Compiles the {elseif} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile ElseIf Class
*/
class Smarty_Internal_Compile_ElseIf extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {elseif} tag
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

        $nesting = $this->_close_tag(array('if', 'elseif'));

        if (empty($this->compiler->prefix_code)) {
            $this->_open_tag('elseif', $nesting);
            return '<?php }elseif(' . $args['if condition'] . '){?>';
        } else {
            $tmp = '';
            foreach ($this->compiler->prefix_code as $code) $tmp .= $code;
            $this->compiler->prefix_code = array();
            $this->_open_tag('elseif', $nesting + 1);
            return '<?php }else{?>' . $tmp . '<?php if (' . $args['if condition'] . '){?>';
        } 
    } 
} 

?>
