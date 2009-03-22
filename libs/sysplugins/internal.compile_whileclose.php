<?php
/**
* Smarty Internal Plugin Compile While Close
* 
* Compiles the {/while} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
             
/**
* Smarty Internal Plugin Compile WhileClose Class
*/
class Smarty_Internal_Compile_WhileClose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/while} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler; 
        $this->_close_tag(array('while'));
        return "<?php }?>";
    } 
} 

?>
