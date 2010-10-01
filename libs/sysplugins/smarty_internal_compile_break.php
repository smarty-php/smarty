<?php

/**
 * Smarty Internal Plugin Compile Break
 * 
 * Compiles the {break} tag
 * 
 * @package Smarty
 * @subpackage Compiler
 * @author Uwe Tews 
 */
/**
 * Smarty Internal Plugin Compile Break Class
 */
class Smarty_Internal_Compile_Break extends Smarty_Internal_CompileBase {
    /**
     * Compiles code for the {break} tag
     * 
     * @param array $args array with attributes from parser
     * @param object $compiler compiler object
     * @return string compiled code
     */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->smarty = $compiler->smarty;
        $this->optional_attributes = array('levels'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        if (isset($_attr['levels'])) {
            if (!is_numeric($_attr['levels'])) {
                $this->compiler->trigger_template_error('level attribute must be a numeric constant', $this->compiler->lex->taglineno);
            } 
            $_levels = $_attr['levels'];
        } else {
            $_levels = 1;
        } 
        $level_count = $_levels;
        $stack_count = count($compiler->_tag_stack) - 1;
        while ($level_count > 0 && $stack_count >= 0) {
            if (in_array($compiler->_tag_stack[$stack_count][0], array('for', 'foreach', 'while', 'section'))) {
                $level_count--;
            } 
            $stack_count--;
        } 
        if ($level_count != 0) {
            $this->compiler->trigger_template_error("cannot break {$_levels} level(s)", $this->compiler->lex->taglineno);
        } 
        // this tag does not return compiled code
        $this->compiler->has_code = true;
        return "<?php break {$_levels}?>";
    } 
} 

?>