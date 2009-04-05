<?php
/**
* Smarty Internal Plugin Compile For
* 
* Compiles the {for} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile For Class
*/
class Smarty_Internal_Compile_For extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {for} tag
    * 
    * Smarty 3 does implement two different sytaxes:
    * 
    * - {for $var in $array}
    * For looping over arrays or iterators
    * 
    * - {for $x=0; $x<$y; $x++}
    * For general loops
    * 
    * The parser is gereration different sets of attribute by which this compiler can 
    * determin which syntax is used.
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler; 
        // {for $x=0; $x<$y; $x++} syntax
        $this->required_attributes = array('ifexp', 'start', 'loop', 'varloop'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        $this->_open_tag('for');

        $output = "<?php ";
        foreach ($_attr['start'] as $_statement) {
            $output .= " \$_smarty_tpl->tpl_vars[$_statement[var]] = new Smarty_Variable;";
            $output .= " \$_smarty_tpl->tpl_vars[$_statement[var]]->value = $_statement[value];\n";
        } 
        $output .= "  if ($_attr[ifexp]){ for (\$_foo=true;$_attr[ifexp]; \$_smarty_tpl->tpl_vars[$_attr[varloop]]->value$_attr[loop]){\n";
        $output .= "?>"; 
        // return compiled code
        return $output;
    } 
} 

?>
