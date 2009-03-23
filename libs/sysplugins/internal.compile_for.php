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
        if (isset($args['from'])) {
            // {for $var in $array}  syntax
            $this->required_attributes = array('from', 'item'); 
            // check and get attributes
            $_attr = $this->_get_attributes($args);

            $this->_open_tag('forarray');

            $from = $_attr['from'];
            $item = $_attr['item'];

            $output = "<?php ";
            $output .= " \$_smarty_tpl->tpl_vars[$item] = new Smarty_Variable;\n";
            $output .= " \$_from = $from; if (!is_array(\$_from) && !is_object(\$_from)) { settype(\$_from, 'array');}\n";
            $output .= " \$_smarty_tpl->tpl_vars[$item]->total=count(\$_from);\n";
            $output .= " \$_smarty_tpl->tpl_vars[$item]->iteration=0;\n";
            $output .= " \$_smarty_tpl->tpl_vars[$item]->index=-1;\n";
            $output .= "if (\$_smarty_tpl->tpl_vars[$item]->total > 0){\n";
            $output .= "    foreach (\$_from as \$_smarty_tpl->tpl_vars[$item]->key => \$_smarty_tpl->tpl_vars[$item]->value){\n";
            $output .= " \$_smarty_tpl->tpl_vars[$item]->iteration++;\n";
            $output .= " \$_smarty_tpl->tpl_vars[$item]->index++;\n";
            $output .= "?>"; 
            // return compiled code
            return $output;
        } else {
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
} 
?>
