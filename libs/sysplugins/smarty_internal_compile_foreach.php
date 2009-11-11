<?php
/**
* Smarty Internal Plugin Compile Foreach
* 
* Compiles the {foreach} {foreachelse} {/foreach} tags
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
        $tpl = $compiler->template; 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        $this->_open_tag('foreach', array('foreach',$this->compiler->nocache));
		// maybe nocache because of nocache variables
		$this->compiler->nocache = $this->compiler->nocache | $this->compiler->tag_nocache;

        $from = $_attr['from'];
        $item = $_attr['item'];

        if (isset($_attr['key'])) {
            $key = $_attr['key'];
        } else {
            $key = null;
        } 

        if (isset($_attr['name'])) {
            $name = $_attr['name'];
            $has_name = true;
            $SmartyVarName = '$smarty.foreach.' . trim($name,'\'"') . '.';
        } else {
            $name = null;
            $has_name = false;
        } 
        $ItemVarName = '$' . trim($item,'\'"') . '@';
        // evaluates which Smarty variables and properties have to be computed
        if ($has_name) {
            $usesSmartyFirst = strpos($tpl->template_source, $SmartyVarName . 'first') !== false;
            $usesSmartyLast = strpos($tpl->template_source, $SmartyVarName . 'last') !== false;
            $usesSmartyIndex = strpos($tpl->template_source, $SmartyVarName . 'index') !== false;
            $usesSmartyIteration = strpos($tpl->template_source, $SmartyVarName . 'iteration') !== false;
            $usesSmartyShow = strpos($tpl->template_source, $SmartyVarName . 'show') !== false;
            $usesSmartyTotal = $usesSmartyLast || strpos($tpl->template_source, $SmartyVarName . 'total') !== false;
        } else {
            $usesSmartyFirst = false;
            $usesSmartyLast = false;
            $usesSmartyTotal = false;
        }

        $usesPropFirst = $usesSmartyFirst || strpos($tpl->template_source, $ItemVarName . 'first') !== false;
        $usesPropLast = $usesSmartyLast || strpos($tpl->template_source, $ItemVarName . 'last') !== false;
        $usesPropIndex = $usesPropFirst || strpos($tpl->template_source, $ItemVarName . 'index') !== false;
        $usesPropIteration = $usesPropLast || strpos($tpl->template_source, $ItemVarName . 'iteration') !== false;
        $usesPropShow = strpos($tpl->template_source, $ItemVarName . 'show') !== false;
        $usesPropTotal = $usesSmartyTotal || $usesPropLast || strpos($tpl->template_source, $ItemVarName . 'total') !== false; 
        // generate output code
        $output = "<?php ";
        $output .= " \$_smarty_tpl->tpl_vars[$item] = new Smarty_Variable;\n";
        if ($key != null) {
            $output .= " \$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable;\n";
        } 
        $output .= " \$_from = $from; if (!is_array(\$_from) && !is_object(\$_from)) { settype(\$_from, 'array');}\n";
        if ($usesPropTotal) {
            $output .= " \$_smarty_tpl->tpl_vars[$item]->total=count(\$_from);\n";
        } 
        if ($usesPropIteration) {
            $output .= " \$_smarty_tpl->tpl_vars[$item]->iteration=0;\n";
        } 
        if ($usesPropIndex) {
            $output .= " \$_smarty_tpl->tpl_vars[$item]->index=-1;\n";
        } 
        if ($has_name) {
            if ($usesSmartyTotal) {
                $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['total'] = \$_smarty_tpl->tpl_vars[$item]->total;\n";
            } 
            if ($usesSmartyIteration) {
                $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['iteration']=0;\n";
            } 
            if ($usesSmartyIndex) {
                $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['index']=-1;\n";
            } 
        } 
        $output .= "if (count(\$_from) > 0){\n";
        $output .= "    foreach (\$_from as \$_smarty_tpl->tpl_vars[$item]->key => \$_smarty_tpl->tpl_vars[$item]->value){\n";
        if ($key != null) {
            $output .= " \$_smarty_tpl->tpl_vars[$key]->value = \$_smarty_tpl->tpl_vars[$item]->key;\n";
        } 
        if ($usesPropIteration) {
            $output .= " \$_smarty_tpl->tpl_vars[$item]->iteration++;\n";
        } 
        if ($usesPropIndex) {
            $output .= " \$_smarty_tpl->tpl_vars[$item]->index++;\n";
        } 
        if ($usesPropFirst) {
            $output .= " \$_smarty_tpl->tpl_vars[$item]->first = \$_smarty_tpl->tpl_vars[$item]->index === 0;\n";
        } 
        if ($usesPropLast) {
            $output .= " \$_smarty_tpl->tpl_vars[$item]->last = \$_smarty_tpl->tpl_vars[$item]->iteration === \$_smarty_tpl->tpl_vars[$item]->total;\n";
        } 
        if ($has_name) {
            if ($usesSmartyFirst) {
                $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['first'] = \$_smarty_tpl->tpl_vars[$item]->first;\n";
            } 
            if ($usesSmartyIteration) {
                $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['iteration']++;\n";
            } 
            if ($usesSmartyIndex) {
                $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['index']++;\n";
            } 
            if ($usesSmartyLast) {
                $output .= " \$_smarty_tpl->tpl_vars['smarty']->value['foreach'][$name]['last'] = \$_smarty_tpl->tpl_vars[$item]->last;\n";
            } 
        } 
        $output .= "?>";

        return $output;
    } 
} 

/**
* Smarty Internal Plugin Compile Foreachelse Class
*/ 
class Smarty_Internal_Compile_Foreachelse extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {foreachelse} tag
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

        list($_open_tag, $nocache) = $this->_close_tag(array('foreach'));
        $this->_open_tag('foreachelse',array('foreachelse', $nocache));

        return "<?php }} else { ?>";
    } 
} 

/**
* Smarty Internal Plugin Compile Foreachclose Class
*/ 
class Smarty_Internal_Compile_Foreachclose extends Smarty_Internal_CompileBase {
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

		// must endblock be nocache?
		if ($this->compiler->nocache) {
               $this->compiler->tag_nocache = true;
        }

        list($_open_tag, $this->compiler->nocache) = $this->_close_tag(array('foreach', 'foreachelse'));

        if ($_open_tag == 'foreachelse')
            return "<?php } ?>";
        else
            return "<?php }} ?>";
    } 
}

?>
