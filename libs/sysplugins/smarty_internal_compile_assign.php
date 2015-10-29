<?php
/**
 * Smarty Internal Plugin Compile Assign
 * Compiles the {assign} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

/**
 * Smarty Internal Plugin Compile Assign Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Assign extends Smarty_Internal_CompileBase
{
    /**
     * Compiles code for the {assign} tag
     *
     * @param  array                                $args      array with attributes from parser
     * @param \Smarty_Internal_TemplateCompilerBase $compiler  compiler object
     * @param  array                                $parameter array with compilation parameter
     *
     * @return string compiled code
     * @throws \SmartyCompilerException
     */
    public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter)
    {
        // the following must be assigned at runtime because it will be overwritten in Smarty_Internal_Compile_Append
        $this->required_attributes = array('var', 'value');
        $this->shorttag_order = array('var', 'value');
        $this->optional_attributes = array('scope');
        $_nocache = 'null';
        $_scope = Smarty::SCOPE_LOCAL;
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        // nocache ?
        if ($compiler->tag_nocache || $compiler->nocache) {
            $_nocache = 'true';
            // create nocache var to make it know for further compiling
            if (isset($compiler->template->tpl_vars[trim($_attr['var'], "'")])) {
                $compiler->template->tpl_vars[trim($_attr['var'], "'")]->nocache = true;
            } else {
                $compiler->template->tpl_vars[trim($_attr['var'], "'")] = new Smarty_Variable(null, true);
            }
        }
        // scope setup
        if (isset($_attr['scope'])) {
            $_attr['scope'] = trim($_attr['scope'], "'\"");
            if ($_attr['scope'] == 'parent') {
                $_scope = Smarty::SCOPE_PARENT;
            } elseif ($_attr['scope'] == 'root') {
                $_scope = Smarty::SCOPE_ROOT;
            } elseif ($_attr['scope'] == 'global') {
                $_scope = Smarty::SCOPE_GLOBAL;
            } elseif ($_attr['scope'] == 'smarty') {
                $_scope = Smarty::SCOPE_SMARTY;
            } elseif ($_attr['scope'] == 'tpl_root') {
                $_scope = Smarty::SCOPE_TPL_ROOT;
            } else {
                $compiler->trigger_template_error('illegal value for "scope" attribute', null, true);
            }
            $_scope += (isset($_attr['bubble_up']) && $_attr['bubble_up'] == 'false') ? 0 : Smarty::SCOPE_BUBBLE_UP;

        }
        // compiled output
        if (isset($parameter['smarty_internal_index'])) {
            $output = "<?php \$_smarty_tpl->smarty->ext->_var->createLocalArrayVariable(\$_smarty_tpl, $_attr[var], $_nocache);\n\$_smarty_tpl->tpl_vars[$_attr[var]]->value$parameter[smarty_internal_index] = $_attr[value];";
        } else {
            // implement Smarty2's behaviour of variables assigned by reference
            if ($compiler->template->smarty instanceof SmartyBC) {
                $output = "<?php if (isset(\$_smarty_tpl->tpl_vars[$_attr[var]])) {\$_smarty_tpl->tpl_vars[$_attr[var]] = clone \$_smarty_tpl->tpl_vars[$_attr[var]];";
                $output .= "\n\$_smarty_tpl->tpl_vars[$_attr[var]]->value = $_attr[value]; \$_smarty_tpl->tpl_vars[$_attr[var]]->nocache = $_nocache;";
                $output .= "\n} else \$_smarty_tpl->tpl_vars[$_attr[var]] = new Smarty_Variable($_attr[value], $_nocache);";
            } else {
                $output = "<?php \$_smarty_tpl->tpl_vars[$_attr[var]] = new Smarty_Variable($_attr[value], $_nocache);";
            }
        }
        $output .= "\n\$_smarty_tpl->ext->_updateScope->updateScope(\$_smarty_tpl, $_attr[var], $_scope);";
        $output .= '?>';

        return $output;
    }
}
