<?php
/**
* Smarty Internal Plugin Compile Foreach
* 
* Compiles the {foreach} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Foreach Class
*/
class Smarty_Internal_Compile_Foreach extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {foreach} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('from', 'item');
        $this->optional_attributes = array('name', 'key'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        $this->_open_tag('foreach');

        $from = $_attr['from'];
        $item = $_attr['item'];

        if (isset($_attr['key'])) {
            $key = $_attr['key'];
        } else {
            $key = null;
        } 

        if (isset($_attr['name'])) {
            $name = $_attr['name'];
        } else {
            $name = null;
        } 
        $output = "<?php ";
        $output .= " \$_smarty_tpl->tpl_vars[$item] = new Smarty_Variable;\n";
        if ($key != null) {
            $output .= " \$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable;\n";
        } 
        $output .= " \$_from = $from; if (!is_array(\$_from) && !is_object(\$_from)) { settype(\$_from, 'array');}\n";
        $output .= " \$_smarty_tpl->tpl_vars[$item]->total=count(\$_from);\n";
        $output .= " \$_smarty_tpl->tpl_vars[$item]->iteration=0;\n";
        $output .= " \$_smarty_tpl->tpl_vars[$item]->index=-1;\n";
        if ($name != null) {
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['total'] = \$_smarty_tpl->tpl_vars[$item]->total;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['iteration']=0;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['index']=-1;\n";
        } 
        $output .= "if (count(\$_from) > 0){\n";
        $output .= "    foreach (\$_from as \$_smarty_tpl->tpl_vars[$item]->key => \$_smarty_tpl->tpl_vars[$item]->value){\n";
        if ($key != null) {
            $output .= " \$_smarty_tpl->tpl_vars[$key]->value = \$_smarty_tpl->tpl_vars[$item]->key;\n";
        } 
        $output .= " \$_smarty_tpl->tpl_vars[$item]->first = \$_smarty_tpl->tpl_vars[$item]->iteration === 0;\n";
        $output .= " \$_smarty_tpl->tpl_vars[$item]->iteration++;\n";
        $output .= " \$_smarty_tpl->tpl_vars[$item]->index++;\n";
        $output .= " \$_smarty_tpl->tpl_vars[$item]->last = \$_smarty_tpl->tpl_vars[$item]->iteration === \$_smarty_tpl->tpl_vars[$item]->total;\n";
        if ($name != null) {
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['first'] = \$_smarty_tpl->tpl_vars[$item]->first;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['iteration']++;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['index']++;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['last'] = \$_smarty_tpl->tpl_vars[$item]->last;\n";
        } 
        $output .= "?>";

        return $output;
    } 
} 

?>
