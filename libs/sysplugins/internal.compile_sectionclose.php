<?php
/**
* Smarty Internal Plugin Compile Section Close
* 
* Compiles the {/section} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile SectionClose Class
*/
class Smarty_Internal_Compile_SectionClose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/section} tag
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

        $_open_tag = $this->_close_tag(array('section', 'sectionelse'));
        if ($_open_tag == 'sectionelse')
            return "<?php endif; ?>";
        else
            return "<?php endfor; endif; ?>";
    } 
} 

?>
