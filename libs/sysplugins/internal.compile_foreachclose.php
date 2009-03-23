<?php 
/**
* Smarty Internal Plugin Compile Foreach Close
*
* Compiles the {/foreach} tag 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews
*/
/**
* Smarty Internal Plugin Compile ForeachClose Class
*/ 
class Smarty_Internal_Compile_ForeachClose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/foreach} tag
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

        $_open_tag = $this->_close_tag(array('foreach', 'foreachelse'));
        if ($_open_tag == 'foreachelse')
            return "<?php } ?>";
        else
            return "<?php }} ?>";
    } 
}

?>
