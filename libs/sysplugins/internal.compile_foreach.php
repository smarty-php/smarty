<?php 
/**
* Smarty Internal Plugin Compile Foreach
*
* Compiles the {foreach} tag 
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
            $key_part = "\$_smarty_tpl->tpl_vars[$key]->value => ";
        } else {
            $key = null;
            $key_part = '';
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
        $output .= " \$_from = $from; if (\$_from !== false) { if (!is_array(\$_from) && !is_object(\$_from)) { settype(\$_from, 'array');}\n";
        if ($name != null) {
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['total'] = count(\$_from);\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['iteration']=0;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['index']=-1;\n";
        } 
        $output .= "if (count(\$_from) > 0){\n";
        $output .= "    foreach (\$_from as " . $key_part . "\$_smarty_tpl->tpl_vars[$item]->value){\n";
        if ($name != null) {
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['first'] = \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['iteration']===0;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['iteration']++;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['index']++;\n";
            $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['last'] = \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['iteration']=== \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['total'];\n";
        } 
        $output .= "?>";

        return $output;
    } 
} 

?>
