<?php
/**
* Smarty Internal Plugin Compile For Close
* 
* Compiles the {/for} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile ForClose Class
*/
class Smarty_Internal_Compile_ForClose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/for} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler; 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        $_open_tag = $this->_close_tag(array('for', 'forelse'));
        if ($_open_tag == 'forelse')
            return "<?php }  ?>";
        else
            return "<?php }} ?>";
    } 
} 

?>
